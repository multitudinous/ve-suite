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
         void ExecuteCommands();

	      Trackball* GetTrackball();                                        //Accessor for Trackball
         KeyboardMouse* GetKeyboardMouse();                                //Accessor for KeyboardMouse

      private:
         Trackball* trackball;
         KeyboardMouse* keyboard_mouse;

         std::map<std::string,VE_EVENTS::EventHandler*> _eventHandlers;    //The event handler for commands

   };
}

#endif