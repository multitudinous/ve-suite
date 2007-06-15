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
#ifndef GL_ENGINE_H
#define GL_ENGINE_H

#include "VE_Conductor/Framework/UIDialog.h"
#include <wx/glcanvas.h>
#include <GL/glut.h>
#include <wx/image.h>
#include <wx/wx.h>

#include <iostream>
#include <iomanip>
#include <fstream>
#include <ostream>
#include <stdlib.h>
#include <stdio.h>

using namespace std;

class GL_Engine : public wxGLCanvas
{
public:
	GL_Engine( UIDialog* tcontrol, int id, const wxPoint& pos, const wxSize& size, long style, 
						 const wxString& name );
	~GL_Engine();

	UIDialog* _parent;

	void _draw();
	void _onMouse(wxMouseEvent& event);
	void _drawPoint(int,int);
	void _drawShapes();
	void _drawCircle();
	void _drawRect();

	int activepts;
	int pt1[2];
	int pt2[2];
	float DEG2RAD;
	float radius;


  DECLARE_EVENT_TABLE();
};
#endif
