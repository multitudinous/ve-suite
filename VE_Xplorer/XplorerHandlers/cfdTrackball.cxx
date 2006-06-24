/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2006 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
 *   - Reaction Engineering International, www.reaction-eng.com
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 * -----------------------------------------------------------------
 * File:          $RCSfile: filename,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include <iostream>

#include "VE_Xplorer/XplorerHandlers/cfdTrackball.h"

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
