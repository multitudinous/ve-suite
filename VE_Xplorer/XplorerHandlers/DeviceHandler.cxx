#include "VE_Xplorer/XplorerHandlers/DeviceHandler.h"

#include "VE_Xplorer/XplorerHandlers/cfdModelHandler.h"

#include "VE_Xplorer/XplorerHandlers/KeyboardMouse.h"
#include "VE_Xplorer/XplorerHandlers/Wand.h"

#include "VE_Xplorer/XplorerHandlers/EventHandler.h"
#include "VE_Xplorer/XplorerHandlers/DeviceEventHandler.h"
#include "VE_Xplorer/XplorerHandlers/KeyboardMouseEH.h"
#include "VE_Xplorer/XplorerHandlers/ViewEventHandler.h"

#include "VE_Open/XML/Command.h"
#include "VE_Open/XML/DataValuePair.h"

vprSingletonImp( VE_Xplorer::DeviceHandler );

using namespace VE_Xplorer;

////////////////////////////////////////////////////////////////////////////////
DeviceHandler::DeviceHandler()
:
navigation( true )
{
   devices[ std::string( "Wand" ) ] = new VE_Xplorer::Wand();
   devices[ std::string( "KeyboardMouse" ) ] = new VE_Xplorer::KeyboardMouse();

   _eventHandlers[ std::string( "VIEW_SELECTION" ) ] = new VE_EVENTS::ViewEventHandler();
   _eventHandlers[ std::string( "CHANGE_DEVICE_MODE" ) ] = new VE_EVENTS::DeviceEventHandler();
   _eventHandlers[ std::string( "TRACKBALL_PROPERTIES" ) ] = new VE_EVENTS::KeyboardMouseEventHandler();
   
}
////////////////////////////////////////////////////////////////////////////////
void DeviceHandler::CleanUp()
{

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
VE_Xplorer::Device* DeviceHandler::GetActiveDevice()
{
   std::map< std::string, VE_Xplorer::Device* >::iterator currentDevice;
   if( cfdModelHandler::instance()->GetXMLCommand() )
   {
      currentDevice = devices.find( cfdModelHandler::instance()->GetXMLCommand()->GetCommandName() );
   }

   return currentDevice->second;
}
////////////////////////////////////////////////////////////////////////////////
void DeviceHandler::ProcessDeviceEvents()
{
   //Update Device properties
   ExecuteCommands();

   //Get the active device


   // --- Temporary Fix --- //
   this->GetKeyboardMouse()->UpdateNavigation();

}
////////////////////////////////////////////////////////////////////////////////
Wand* DeviceHandler::GetWand()
{
   return dynamic_cast< VE_Xplorer::Wand* >( devices.find( "Wand" )->second );
}
////////////////////////////////////////////////////////////////////////////////
KeyboardMouse* DeviceHandler::GetKeyboardMouse()
{
	return dynamic_cast< VE_Xplorer::KeyboardMouse* >( devices.find( "KeyboardMouse" )->second );
}
////////////////////////////////////////////////////////////////////////////////
