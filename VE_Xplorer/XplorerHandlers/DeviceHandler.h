#ifndef DEVICE_HANDLER
#define DEVICE_HANDLER

/*!\file DeviceHandler.h
DeviceHandler API
*/
/*!\class VE_Xplorer::DeviceHandler
* 
*/

#include "VE_Installer/include/VEConfig.h"

#include <vpr/Util/Singleton.h>

//C/C++ Libraries
#include <string>
#include <map>

namespace VE_Xplorer
{
   class KeyboardMouse;
	class Wand;
}

namespace VE_EVENTS
{
   class EventHandler;
}

namespace VE_Xplorer
{
class VE_XPLORER_EXPORTS DeviceHandler
{
public:
	DeviceHandler();
   ~DeviceHandler(){;}
   vprSingletonHeader( DeviceHandler );

   void CleanUp();
   void ExecuteCommands();
   void ProcessDeviceEvents();

   void SetMode( unsigned int mode );

   unsigned int GetMode();

   KeyboardMouse* GetKeyboardMouse();
	Wand* GetWand();

private:
   KeyboardMouse* keyboard_mouse;
	Wand* wand;

   unsigned int device_mode;

   std::map< std::string, VE_EVENTS::EventHandler* > _eventHandlers;

};
}

#endif //DEVICE_HANDLER_H
