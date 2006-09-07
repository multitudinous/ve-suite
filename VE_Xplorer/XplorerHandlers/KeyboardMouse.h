#ifndef KEYBOARD_MOUSE_H
#define KEYBOARD_MOUSE_H

#include <boost/shared_ptr.hpp>
#include <gadget/Type/KeyboardMouseInterface.h>
#include <gadget/Type/KeyboardMouse/KeyEvent.h>
#include <gadget/Type/KeyboardMouse/MouseEvent.h>

#include "VE_Installer/include/VEConfig.h"

using namespace boost;

namespace VE_Xplorer
{
   class VE_XPLORER_EXPORTS KeyboardMouse
   {
      public:
         KeyboardMouse();
	      ~KeyboardMouse();

	      void preFrame();
         void SetKey(int key);
         int GetKey();

         gadget::KeyboardMouse::EventQueue evt_queue;

      private:
         gadget::KeyboardMouseInterface mKeyboard;

         int _key;
   };
}

#endif
