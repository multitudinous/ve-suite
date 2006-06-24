#include <iostream>

#include "VE_Xplorer/XplorerHandler/cfdTrackball.h"

#include <vrj/Draw/OGL/GlDrawManager.h>
#include <vrj/Draw/OGL/GlWindow.h>
#include <vrj/Display/Viewport.h>

using namespace VE_Xplorer;

cfdTrackball::cfdTrackball(){
	mKeyboard.init("VJKeyboard");
	tb.Init();
}

cfdTrackball::~cfdTrackball(){}

void cfdTrackball::Reshape(unsigned int width,unsigned int height){
	tb.Reshape(width,height);
}

void cfdTrackball::SetFOVy(float _top,float _bottom,float _near){
	tb.SetFOVy(_top,_bottom,_near);
}

void cfdTrackball::preFrame(){
	gadget::KeyboardMouse::EventQueue evt_queue=mKeyboard->getEventQueue();
	gadget::KeyboardMouse::EventQueue::iterator i;

   if ( evt_queue.empty() )
   {
      return;
   }

	for(i=evt_queue.begin();i!=evt_queue.end();++i){
		const gadget::EventType type=(*i)->type();

		if(type==gadget::KeyPressEvent){
			gadget::KeyEventPtr key_evt=dynamic_pointer_cast<gadget::KeyEvent>(*i);
			tb.Keyboard(key_evt->getKey());
		}
		else if(type==gadget::MouseButtonPressEvent){
			gadget::MouseEventPtr mouse_evt=dynamic_pointer_cast<gadget::MouseEvent>(*i);
			tb.Mouse(mouse_evt->getButton(),1,mouse_evt->getX(),mouse_evt->getY());
		}
		else if(type==gadget::MouseButtonReleaseEvent){
			gadget::MouseEventPtr mouse_evt=dynamic_pointer_cast<gadget::MouseEvent>(*i);
			tb.Mouse(mouse_evt->getButton(),0,mouse_evt->getX(),mouse_evt->getY());
		}
		else if(type==gadget::MouseMoveEvent){
			gadget::MouseEventPtr mouse_evt=dynamic_pointer_cast<gadget::MouseEvent>(*i);
			tb.Motion(mouse_evt->getX(),mouse_evt->getY());
		}
	}

	tb.Matrix();
}
