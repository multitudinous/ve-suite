#ifndef DEVICE_HANDLER
#define DEVICE_HANDLER

/*!\file DeviceHandler.h
DeviceHandler API
*/
/*!\class VE_Xplorer::DeviceHandler
* 
*/

#include <string>
#include <vector>
#include <map>

#include <vpr/Util/Singleton.h>
#include "VE_Installer/include/VEConfig.h"

namespace VE_Xplorer
{
   class Trackball;
   class KeyboardMouse;
}

namespace VE_EVENTS
{
   class EventHandler;
}

namespace VE_Xplorer
{
   class VE_XPLORER_EXPORTS DeviceHandler                                  //public vpr::Singleton<DeviceHandler>
   {
      public:
         DeviceHandler();
         ~DeviceHandler(){;}                                               //Never gets called, don't implement
         vprSingletonHeader(DeviceHandler);

         void CleanUp();
         void ExecuteCommands();                                           //Execute commands from VE_Conductor
         void ProcessDeviceEvents();                                       //Process all events for active device

         void SetMode(unsigned int mode);                                  //Set the selected device mode

         unsigned int GetMode();                                           //Return the selected device mode

	      Trackball* GetTrackball();                                        //Accessor for Trackball
         KeyboardMouse* GetKeyboardMouse();                                //Accessor for KeyboardMouse

      private:
         Trackball* trackball;
         KeyboardMouse* keyboard_mouse;

         unsigned int device_mode;                                         //Tells which device is selected

         std::map<std::string,VE_EVENTS::EventHandler*> _eventHandlers;    //The event handler for commands

   };
}

#endif