#include "VE_Xplorer/XplorerHandlers/DeviceHandler.h"

#include "VE_Xplorer/XplorerHandlers/cfdModelHandler.h"

#include "VE_Xplorer/XplorerHandlers/KeyboardMouse.h"
#include "VE_Xplorer/XplorerHandlers/Wand.h"

#include "VE_Xplorer/XplorerHandlers/EventHandler.h"
#include "VE_Xplorer/XplorerHandlers/DeviceEH.h"
#include "VE_Xplorer/XplorerHandlers/DeviceModeEH.h"
#include "VE_Xplorer/XplorerHandlers/KeyboardMouseEH.h"
#include "VE_Xplorer/XplorerHandlers/ViewEventHandler.h"

#include "VE_Open/XML/Command.h"
#include "VE_Open/XML/DataValuePair.h"

vprSingletonImp( VE_Xplorer::DeviceHandler );

using namespace VE_Xplorer;

////////////////////////////////////////////////////////////////////////////////
DeviceHandler::DeviceHandler()
:
device_mode( 0 )
{
   devices[ std::string( "Wand" ) ] = new VE_Xplorer::Wand();
   devices[ std::string( "KeyboardMouse" ) ] = new VE_Xplorer::KeyboardMouse();

   active_device = devices.find( "KeyboardMouse" )->second;

   _eventHandlers[ std::string( "VIEW_SELECTION" ) ] = new VE_EVENTS::ViewEventHandler();
   _eventHandlers[ std::string( "CHANGE_DEVICE" ) ] = new VE_EVENTS::DeviceEventHandler();
   _eventHandlers[ std::string( "CHANGE_DEVICE_MODE" ) ] = new VE_EVENTS::DeviceModeEventHandler();
   _eventHandlers[ std::string( "TRACKBALL_PROPERTIES" ) ] = new VE_EVENTS::KeyboardMouseEventHandler();
   
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
         currentEventHandler->second->SetGlobalBaseObject();
         currentEventHandler->second->Execute( cfdModelHandler::instance()->GetXMLCommand() );
      }
   }
}
////////////////////////////////////////////////////////////////////////////////
void DeviceHandler::SetActiveDevice( std::string device )
{
   active_device = devices.find( device )->second;
}
////////////////////////////////////////////////////////////////////////////////
void DeviceHandler::SetDeviceMode( unsigned int mode )
{
   device_mode = mode;
}
////////////////////////////////////////////////////////////////////////////////
void DeviceHandler::ProcessDeviceEvents()
{
   //Update Device properties
   ExecuteCommands();

   if( device_mode == 0 )
   {
      active_device->UpdateNavigation();
   }

   else if( device_mode == 1 )
   {
      active_device->UpdateSelection();
   }

}
////////////////////////////////////////////////////////////////////////////////
Wand* DeviceHandler::GetWand()
{
   return static_cast< VE_Xplorer::Wand* >( devices.find( "Wand" )->second );
}
////////////////////////////////////////////////////////////////////////////////
KeyboardMouse* DeviceHandler::GetKeyboardMouse()
{
	return static_cast< VE_Xplorer::KeyboardMouse* >( devices.find( "KeyboardMouse" )->second );
}
////////////////////////////////////////////////////////////////////////////////
