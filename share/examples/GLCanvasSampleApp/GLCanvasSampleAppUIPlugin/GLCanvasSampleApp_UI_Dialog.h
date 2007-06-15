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
#ifndef GLCanvasSampleApp_UI_DIALOG_H
#define GLCanvasSampleApp_UI_DIALOG_H
#include "VE_Conductor/Framework/UIDialog.h"
#include "GL_Engine.h"
#include <wx/glcanvas.h>
#include <GL/glut.h>
#include <vector>
#include <string>

using namespace std;

enum {
  GEOM_SHAPE_RADIOBOX,
	DESIGN_BUTTON,
	RESET_BUTTON
};

class GLCanvasSampleApp_UI_Dialog : public UIDialog
{
   public:
  GLCanvasSampleApp_UI_Dialog(wxWindow* parent, int id,
          double* radius,
          double* length,
          double* width,
          double* xcoord,
          double* ycoord,
          long* type);
      GLCanvasSampleApp_UI_Dialog() {;}
  
      virtual ~GLCanvasSampleApp_UI_Dialog();
  
      virtual bool TransferDataFromWindow();
      virtual bool TransferDataToWindow();
      virtual void Lock(bool l); 

      void _onGeomMethod(wxCommandEvent& event);
		void _onActCanvas(wxCommandEvent& event);
		void _onResetCanvas(wxCommandEvent& event);

  // protected:
      //UI widgets variables
   wxRadioBox* _selgeomRBox;
   wxButton*   _updateButton;
	wxButton*   _resetButton;
	wxButton*   _designButton;
	GL_Engine*  _designCanvas;

public:
  double* p_radius;
  double* p_length;
  double* p_width;
  double* p_xcoord;
  double* p_ycoord;
  long* p_type;
      //GUI Variables

  		DECLARE_EVENT_TABLE();
};

#endif

