#include "VE_Xplorer/XplorerHandlers/DeviceHandler.h"

#include "VE_Xplorer/XplorerHandlers/cfdModelHandler.h"

#include "VE_Xplorer/XplorerHandlers/KeyboardMouse.h"
#include "VE_Xplorer/Xplorerhandlers/Wand.h"

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
{
   keyboard_mouse = new VE_Xplorer::KeyboardMouse();
	wand = new VE_Xplorer::Wand();

   device_mode = 0;

   _eventHandlers[ std::string( "VIEW_SELECTION" ) ] = new VE_EVENTS::ViewEventHandler();
   _eventHandlers[ std::string( "CHANGE_DEVICE_MODE" ) ] = new VE_EVENTS::DeviceEventHandler();
   _eventHandlers[ std::string( "TRACKBALL_PROPERTIES" ) ] = new VE_EVENTS::KeyboardMouseEventHandler();
   
}
////////////////////////////////////////////////////////////////////////////////
void DeviceHandler::CleanUp()
{
   if( keyboard_mouse )
	{
      delete keyboard_mouse;
   }

	if( wand )
	{
      delete wand;
   }
}
////////////////////////////////////////////////////////////////////////////////
void DeviceHandler::ExecuteCommands()
{
   std::map<std::string,VE_EVENTS::EventHandler*>::iterator currentEventHandler;
   if(cfdModelHandler::instance()->GetXMLCommand()){
      currentEventHandler=_eventHandlers.find(cfdModelHandler::instance()->GetXMLCommand()->GetCommandName());
      if(currentEventHandler!=_eventHandlers.end()){
         currentEventHandler->second->SetGlobalBaseObject();
         currentEventHandler->second->Execute(cfdModelHandler::instance()->GetXMLCommand());
      }
   }
}
////////////////////////////////////////////////////////////////////////////////
void DeviceHandler::ProcessDeviceEvents()
{
   //Update Device Properties
   ExecuteCommands();

   //Update Device events
   switch( device_mode )
	{
   
      //Update Trackball events
      case 0: keyboard_mouse->UpdateNavigation();

      //case 1:
   

      //Update MouseSelection events
      //mouse_selection->SelectObjects();
   }
}
////////////////////////////////////////////////////////////////////////////////
void DeviceHandler::SetMode( unsigned int mode )
{
   device_mode = mode;
}
////////////////////////////////////////////////////////////////////////////////
unsigned int DeviceHandler::GetMode()
{
   return device_mode;
}
////////////////////////////////////////////////////////////////////////////////
KeyboardMouse* DeviceHandler::GetKeyboardMouse()
{
	return keyboard_mouse;
}
////////////////////////////////////////////////////////////////////////////////
Wand* DeviceHandler::GetWand()
{
	return wand;
}
////////////////////////////////////////////////////////////////////////////////