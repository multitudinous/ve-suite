/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2007 by Iowa State University
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
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "GLCanvasSample_UI_Dialog.h"
#include "GL_Engine.h"
#include <math.h>


BEGIN_EVENT_TABLE(GL_Engine, wxGLCanvas)
  EVT_MOUSE_EVENTS  (GL_Engine::_onMouse)
END_EVENT_TABLE()

///////////////
//Constructor//
///////////////
GL_Engine::GL_Engine(UIDialog* tcontrol, wxWindowID id, const wxPoint& pos, const wxSize& size, long style, const wxString& name)
	:wxGLCanvas(tcontrol, id, pos, size, style, name)
{
	_parent = tcontrol;
  gluOrtho2D(0.0, 500, 0.0, 500);

	activepts = 0;
	DEG2RAD = 3.14159/180;
	radius = 0;

	for ( int i=0; i<2; i++ )
	{
		pt1[i] = 0;
		pt2[i] = 0;
	}
}

//Destructor
GL_Engine::~GL_Engine()
{
}

void GL_Engine::_draw()
{  
  glClearColor(1.0, 1.0, 1.0, 0.0);
  glClear( GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT );
  glColor3f(0.0, 0.0, 0.0);	
}

void GL_Engine::_onMouse(wxMouseEvent& event)
{
  int xpos=0;
  int ypos=0;

  if (event.GetEventType() == wxEVT_RIGHT_DOWN && activepts < 2)
  {
		activepts += 1;
		event.GetPosition(&xpos, &ypos);
		ypos = 496 - ypos;
		if ( activepts == 1 )
		{
			pt1[0] = xpos;
			pt1[1] = ypos;
			_drawPoint( pt1[0], pt1[1] );
		}
		else if ( activepts == 2 )
		{
			pt2[0] = xpos;
			pt2[1] = ypos;
			_drawPoint( pt2[0], pt2[1] );
			_drawShapes();
		}
	}

}
 
void GL_Engine::_drawPoint( int x, int y )
{
		SetCurrent();
		glColor3f(0.0, 0.0, 1.0);
		glPointSize(4.0);
		glBegin(GL_POINTS);
			glVertex2f(x, y);
		glEnd();
    SwapBuffers();
}

void GL_Engine::_drawShapes( void )
{
	if ( ((GLCanvasSample_UI_Dialog *)_parent)->_selgeomRBox->GetSelection() == 0 )
	{
		radius = sqrt( pow( ( pt2[0] - pt1[0] ), 2 ) + pow( ( pt2[1] - pt1[1] ), 2 ) );
		_drawCircle();
	}
	else if ( ((GLCanvasSample_UI_Dialog *)_parent)->_selgeomRBox->GetSelection() == 1  )
	{
		_drawRect();
	}
}

void GL_Engine::_drawCircle( void )
{
	SetCurrent();
	glColor3f(0.0, 1.0, 0.0);
  glBegin(GL_LINE_LOOP);
 
  for (int i=0; i < 360; i++)
  {
     float degInRad = i*DEG2RAD;
     glVertex2f(cos(degInRad)*radius + pt1[0], sin(degInRad)*radius + pt1[1]);
  }
  glEnd();
  SwapBuffers();
}

void GL_Engine::_drawRect( void )
{
	SetCurrent();
	glColor3f(0.0, 1.0, 0.0);
	glBegin(GL_LINES);
		glVertex2f( pt1[0], pt1[1] );
		glVertex2f( pt1[0], pt1[1] + ( pt2[1] - pt1[1] ) );
		glVertex2f( pt1[0], pt1[1] );
		glVertex2f( pt1[0] + ( pt2[0] - pt1[0] ), pt1[1] ) ;
		glVertex2f( pt2[0], pt2[1] );
		glVertex2f( pt2[0], pt2[1] - ( pt2[1] - pt1[1] ) );
		glVertex2f( pt2[0], pt2[1] );
		glVertex2f( pt2[0] - ( pt2[0] - pt1[0] ), pt2[1] );
	glEnd();
	SwapBuffers();
}


