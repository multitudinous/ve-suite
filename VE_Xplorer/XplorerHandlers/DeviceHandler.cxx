/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2007 by Iowa State University
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
 *************** <auto-copyright.pl END do not edit this line> ***************/
// --- VE-Suite Includes --- //
#include "VE_Xplorer/XplorerHandlers/DeviceHandler.h"

#include "VE_Xplorer/XplorerHandlers/cfdModelHandler.h"

#include "VE_Xplorer/XplorerHandlers/KeyboardMouse.h"
#include "VE_Xplorer/XplorerHandlers/Wand.h"
#include "VE_Xplorer/XplorerHandlers/Tablet.h"

#include "VE_Xplorer/XplorerHandlers/EventHandler.h"
#include "VE_Xplorer/XplorerHandlers/DeviceEH.h"
#include "VE_Xplorer/XplorerHandlers/DeviceModeEH.h"
#include "VE_Xplorer/XplorerHandlers/UnselectObjectsEventHandler.h"
#include "VE_Xplorer/XplorerHandlers/CenterPointJumpEventHandler.h"
#include "VE_Xplorer/XplorerHandlers/KeyboardMouseEH.h"
#include "VE_Xplorer/XplorerHandlers/NavigationDataEventHandler.h"

#include "VE_Xplorer/SceneGraph/SceneManager.h"

#include "VE_Open/XML/Command.h"
#include "VE_Open/XML/DataValuePair.h"

#include <osg/BoundingSphere>

vprSingletonImp( VE_Xplorer::DeviceHandler );

using namespace VE_Xplorer;

////////////////////////////////////////////////////////////////////////////////
DeviceHandler::DeviceHandler()
:
activeDCS( VE_SceneGraph::SceneManager::instance()->GetWorldDCS() ),
selectedDCS( 0 ),
device_mode( "World Navigation" ),
center_point( 0, 0, 0 ),
m_threshold( 0.5f ),
m_jump( 10.0f )
{
    //Move the world DCS forward a little bit
    activeDCS->setPosition( osg::Vec3d( 0.0, 2.0, 0.0 ) );

    //Initialize Devices
    devices[ std::string( "Tablet" ) ] = new VE_Xplorer::Tablet();
    devices[ std::string( "Wand" ) ] = new VE_Xplorer::Wand();
    devices[ std::string( "KeyboardMouse" ) ] = new VE_Xplorer::KeyboardMouse();

    //Set properties in Devices
    std::map< std::string, VE_Xplorer::Device* >::const_iterator itr;
    for( itr = devices.begin(); itr != devices.end(); itr++ )
    {
        itr->second->SetActiveDCS( activeDCS.get() );
        itr->second->SetSelectedDCS( selectedDCS.get() );
        itr->second->SetCenterPoint( &center_point );
        itr->second->SetCenterPointThreshold( &m_threshold );
        itr->second->SetCenterPointJump( &m_jump );
    }

    active_device = devices[ "KeyboardMouse" ];

    _eventHandlers[ std::string( "CHANGE_DEVICE" ) ] = new VE_EVENTS::DeviceEventHandler();
    _eventHandlers[ std::string( "CHANGE_DEVICE_MODE" ) ] = new VE_EVENTS::DeviceModeEventHandler();
    _eventHandlers[ std::string( "UNSELECT_OBJECTS" ) ] = new VE_EVENTS::UnselectObjectsEventHandler();
    _eventHandlers[ std::string( "CHANGE_CENTERPOINT_MODE" ) ] = new VE_EVENTS::CenterPointJumpEventHandler();
    _eventHandlers[ std::string( "TRACKBALL_PROPERTIES" ) ] = new VE_EVENTS::KeyboardMouseEventHandler();
    _eventHandlers[ std::string( "Navigation_Data" ) ] = new VE_EVENTS::NavigationDataEventHandler();
}
////////////////////////////////////////////////////////////////////////////////
DeviceHandler::~DeviceHandler()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void DeviceHandler::CleanUp()
{
    //Delete devices in map
    std::map< std::string, VE_Xplorer::Device* >::iterator itr;
    for( itr = devices.begin(); itr != devices.end(); )
    {
        devices.erase( itr++ );
    }

    devices.clear();
}
////////////////////////////////////////////////////////////////////////////////
void DeviceHandler::ExecuteCommands()
{
    std::map< std::string, VE_EVENTS::EventHandler* >::iterator currentEventHandler;
    if( cfdModelHandler::instance()->GetXMLCommand() )
    {
        currentEventHandler = _eventHandlers.find( cfdModelHandler::instance()->GetXMLCommand()->GetCommandName() );

        if( currentEventHandler != _eventHandlers.end() )
        {
            //Set properties in Devices
            std::map< std::string, VE_Xplorer::Device* >::const_iterator itr;
            for( itr = devices.begin(); itr != devices.end(); itr++ )
            {
                itr->second->SetActiveDCS( activeDCS.get() );
                itr->second->SetSelectedDCS( selectedDCS.get() );
            }

            currentEventHandler->second->SetGlobalBaseObject( active_device );
            currentEventHandler->second->Execute( cfdModelHandler::instance()->GetXMLCommand() );
            //Tablet and Wand is always active and need updated...
            if( cfdModelHandler::instance()->GetXMLCommand()->GetCommandName() == "Navigation_Data" )
            {
                currentEventHandler->second->SetGlobalBaseObject( devices[ "Tablet" ] );
                currentEventHandler->second->Execute( cfdModelHandler::instance()->GetXMLCommand() );
                currentEventHandler->second->SetGlobalBaseObject( devices[ "Wand" ] );
                currentEventHandler->second->Execute( cfdModelHandler::instance()->GetXMLCommand() );
            }
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void DeviceHandler::SetActiveDevice( std::string device )
{
    std::map< std::string, VE_Xplorer::Device* >::iterator itr = devices.find( device );
    if ( itr != devices.end() )
    {
        active_device = devices[ device ];
    }
}
////////////////////////////////////////////////////////////////////////////////
void DeviceHandler::SetDeviceMode( std::string mode )
{
    device_mode = mode;

    if( device_mode == "World Navigation" )
    {
        activeDCS = VE_SceneGraph::SceneManager::instance()->GetWorldDCS();
        active_device->SetActiveDCS( activeDCS.get() );
    }
    else if( device_mode == "Object Navigation" )
    {
        if( selectedDCS.valid() )
        {
            activeDCS = selectedDCS;
        }

        active_device->SetActiveDCS( activeDCS.get() );
    }
}
////////////////////////////////////////////////////////////////////////////////
void DeviceHandler::SetCenterPointJumpMode( std::string mode )
{
    if( mode == "Small" )
    {
        m_jump = 10.0;
    }
    else if( mode == "Medium" )
    {
        m_jump = 100.0;
    }
    else if( mode == "Large" )
    {
        m_jump = 1000.0;
    }
    else if( mode == "Bounding Box" )
    {
        m_jump = activeDCS->getBound().radius();
    }
}
////////////////////////////////////////////////////////////////////////////////
void DeviceHandler::UnselectObjects()
{
    activeDCS = VE_SceneGraph::SceneManager::instance()->GetWorldDCS();
    active_device->SetActiveDCS( activeDCS.get() );

    selectedDCS = 0;
    active_device->SetSelectedDCS( selectedDCS.get() );
}
////////////////////////////////////////////////////////////////////////////////
void DeviceHandler::ProcessDeviceEvents()
{
    //Update Device properties
    this->ExecuteCommands();

    if ( device_mode == "World Navigation" || device_mode == "Object Navigation" )
    {
        devices[ "Wand" ]->UpdateNavigation();
        devices[ "KeyboardMouse" ]->UpdateNavigation();

        if( ( active_device != devices[ "Wand" ] ) && ( active_device != devices[ "KeyboardMouse" ] ) )
        {
            active_device->UpdateNavigation();
        }
    }
    else if ( device_mode == "Selection" )
    {
        active_device->UpdateSelection();
    }

    //Get the active dcs from the active device
    activeDCS = active_device->GetActiveDCS();

    //Get the selected dcs from the active device
    selectedDCS = active_device->GetSelectedDCS();

    //Always do this be default
    devices[ "Tablet" ]->UpdateNavigation();
}
////////////////////////////////////////////////////////////////////////////////
VE_Xplorer::Device* DeviceHandler::GetDevice( std::string device )
{
    return devices[ device ];
}
////////////////////////////////////////////////////////////////////////////////
