//!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//Don't implement this class; it is handled through DeviceHandler
//!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#ifndef KEYBOARD_MOUSE_H
#define KEYBOARD_MOUSE_H

/*!\file KeyboardMouse.h
KeyboardMouse API
*/
/*!\class VE_XPlorer::KeyboardMouse
* 
*/

#include <boost/shared_ptr.hpp>
#include <gadget/Type/KeyboardMouseInterface.h>
#include <gadget/Type/KeyboardMouse/KeyEvent.h>
#include <gadget/Type/KeyboardMouse/MouseEvent.h>

#include "VE_Xplorer/XplorerHandlers/Device.h"

#include "VE_Installer/include/VEConfig.h"

namespace VE_Xplorer
{
   class VE_XPLORER_EXPORTS KeyboardMouse : public Device
   {
      public:
         KeyboardMouse();
	      ~KeyboardMouse();

	      void ProcessKeyboardMouseEvents();

         void SetKey(int key);

         int GetKey();
         int GetButton();
         int GetState();
         int GetX();
         int GetY();

         gadget::KeyboardMouse::EventQueue evt_queue;

      private:
         gadget::KeyboardMouseInterface mKeyboard;
         
         int _key;
         int _button;
         int _state;
         int _x;
         int _y;
   };
}

#endif
