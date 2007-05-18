/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2006 by Iowa State University
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
// --- VE-Suite Stuff --- //
#include "VE_Xplorer/XplorerHandlers/DeviceHandler.h"

#include "VE_Xplorer/XplorerHandlers/cfdModelHandler.h"

#include "VE_Xplorer/XplorerHandlers/KeyboardMouse.h"
#include "VE_Xplorer/XplorerHandlers/Wand.h"
#include "VE_Xplorer/XplorerHandlers/Tablet.h"

#include "VE_Xplorer/XplorerHandlers/EventHandler.h"
#include "VE_Xplorer/XplorerHandlers/DeviceEH.h"
#include "VE_Xplorer/XplorerHandlers/DeviceModeEH.h"
#include "VE_Xplorer/XplorerHandlers/KeyboardMouseEH.h"
#include "VE_Xplorer/XplorerHandlers/NavigationDataEventHandler.h"

#include "VE_Xplorer/SceneGraph/SceneManager.h"

#include "VE_Open/XML/Command.h"
#include "VE_Open/XML/DataValuePair.h"

vprSingletonImp( VE_Xplorer::DeviceHandler );

using namespace VE_Xplorer;

////////////////////////////////////////////////////////////////////////////////
DeviceHandler::DeviceHandler()
:
device_mode( "Navigation" )
{
   devices[ std::string( "Tablet" ) ] = new VE_Xplorer::Tablet();
   devices[ std::string( "Tablet" ) ]->SetCenterPoint( &center_point );
   devices[ std::string( "Wand" ) ] = new VE_Xplorer::Wand();
   devices[ std::string( "Wand" ) ]->SetCenterPoint( &center_point );
   devices[ std::string( "KeyboardMouse" ) ] = new VE_Xplorer::KeyboardMouse();
   devices[ std::string( "KeyboardMouse" ) ]->SetCenterPoint( &center_point );
   
   active_device = devices[ "KeyboardMouse" ];

   _eventHandlers[ std::string( "CHANGE_DEVICE" ) ] = new VE_EVENTS::DeviceEventHandler();
   _eventHandlers[ std::string( "CHANGE_DEVICE_MODE" ) ] = new VE_EVENTS::DeviceModeEventHandler();
   _eventHandlers[ std::string( "TRACKBALL_PROPERTIES" ) ] = new VE_EVENTS::KeyboardMouseEventHandler();
   _eventHandlers[ std::string( "Navigation_Data" ) ] = new VE_EVENTS::NavigationDataEventHandler();
   
   activeDCS = VE_SceneGraph::SceneManager::instance()->GetWorldDCS();

   center_point.mData[1] = activeDCS->GetMat().mData[6] = static_cast< VE_Xplorer::KeyboardMouse* >( active_device )->GetCPThreshold();
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
}
////////////////////////////////////////////////////////////////////////////////
void DeviceHandler::ProcessDeviceEvents()
{
   //Update Device properties
   this->ExecuteCommands();

   if ( device_mode == "Navigation" )
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
   //get the active dcs from the active device
   activeDCS = active_device->GetActiveDCS();
   //Always do this be default
   devices[ "Tablet" ]->SetActiveDCS( activeDCS.get() );
   devices[ "Tablet" ]->UpdateNavigation();
}
////////////////////////////////////////////////////////////////////////////////
VE_Xplorer::Device* DeviceHandler::GetDevice( std::string device )
{
   return devices[ device ];
}
////////////////////////////////////////////////////////////////////////////////
