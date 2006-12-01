#include "VE_Xplorer/XplorerHandlers/DeviceHandler.h"

#include "VE_Xplorer/XplorerHandlers/cfdModelHandler.h"

#include "VE_Xplorer/XplorerHandlers/Trackball.h"
#include "VE_Xplorer/XplorerHandlers/KeyboardMouse.h"

#include "VE_Xplorer/XplorerHandlers/EventHandler.h"
#include "VE_Xplorer/XplorerHandlers/TrackballEventHandler.h"

#include "VE_Open/XML/Command.h"
#include "VE_Open/XML/DataValuePair.h"

vprSingletonImp(VE_Xplorer::DeviceHandler);

using namespace VE_Xplorer;

////////////////////////////////////////////////////////////////////////////////
DeviceHandler::DeviceHandler()
{
   trackball=new VE_Xplorer::Trackball();
   keyboard_mouse=new VE_Xplorer::KeyboardMouse();

   _eventHandlers[std::string("TRACKBALL_PROPERTIES")]=new VE_EVENTS::TrackballEventHandler();
}
////////////////////////////////////////////////////////////////////////////////
void DeviceHandler::CleanUp()
{
   if(trackball){
      delete trackball;
   }

   if(keyboard_mouse){
      delete keyboard_mouse;
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
Trackball* DeviceHandler::GetTrackball()
{
	return trackball;
}
////////////////////////////////////////////////////////////////////////////////
KeyboardMouse* DeviceHandler::GetKeyboardMouse()
{
	return keyboard_mouse;
}
////////////////////////////////////////////////////////////////////////////////