#include <iostream>

#include "VE_Xplorer/XplorerHandlers/KeyboardMouse.h"

using namespace VE_Xplorer;

KeyboardMouse::KeyboardMouse(){
	mKeyboard.init("VJKeyboard");

   _key=-1;
}

KeyboardMouse::~KeyboardMouse(){}

void KeyboardMouse::preFrame(){
	evt_queue=mKeyboard->getEventQueue();
   gadget::KeyboardMouse::EventQueue::iterator i;

   if(evt_queue.empty()){
      return;
   }

   for(i=evt_queue.begin();i!=evt_queue.end();++i){
		const gadget::EventType type=(*i)->type();

		if(type==gadget::KeyPressEvent){
			gadget::KeyEventPtr key_evt=dynamic_pointer_cast<gadget::KeyEvent>(*i);
         _key=key_evt->getKey();
		}

      // Use this call if you want to hold a key for it to be active
      /*else if(type==gadget::KeyReleaseEvent){
         _key=-1;
      }*/

      /*
		else if(type==gadget::MouseButtonPressEvent){
			gadget::MouseEventPtr mouse_evt=dynamic_pointer_cast<gadget::MouseEvent>(*i);
		}
		else if(type==gadget::MouseButtonReleaseEvent){
			gadget::MouseEventPtr mouse_evt=dynamic_pointer_cast<gadget::MouseEvent>(*i);
		}
		else if(type==gadget::MouseMoveEvent){
			gadget::MouseEventPtr mouse_evt=dynamic_pointer_cast<gadget::MouseEvent>(*i);
		}
      */
	}
}

void KeyboardMouse::SetKey(int key){
   _key=key;
}

int KeyboardMouse::GetKey(){
   return _key;
}

