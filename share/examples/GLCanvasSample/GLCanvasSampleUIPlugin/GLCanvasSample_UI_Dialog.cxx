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
#include <math.h>

BEGIN_EVENT_TABLE(GLCanvasSample_UI_Dialog, UIDialog)
	EVT_BUTTON		(DESIGN_BUTTON,					    GLCanvasSample_UI_Dialog::_onActCanvas)
  EVT_RADIOBOX	(GEOM_SHAPE_RADIOBOX,		    GLCanvasSample_UI_Dialog::_onGeomMethod)
	EVT_BUTTON		(RESET_BUTTON,              GLCanvasSample_UI_Dialog::_onResetCanvas)
END_EVENT_TABLE()


//Here is the constructor with passed in pointers
GLCanvasSample_UI_Dialog
::GLCanvasSample_UI_Dialog
(wxWindow* parent, int id,
  double* radius,
  double* length,
  double* width,
  long* type)
: UIDialog((wxWindow *) parent, id, "GLCanvasSample"),
  p_radius(radius),
  p_length(length),
  p_width(width),
  p_type(type)
{
	(*p_radius) = 0;
	(*p_length) = 0;
	(*p_width) = 0;
	(*p_type) = 0;

	wxString geomstr[] = { wxT("Circle"),
												 wxT("Rectangle")};

	_selgeomRBox = new wxRadioBox(this, GEOM_SHAPE_RADIOBOX, wxT("Select The Geometry To Be Drawn"),
                                                  wxDefaultPosition, wxDefaultSize, 2,
                                                     geomstr, 1, wxRA_SPECIFY_COLS);

	_updateButton = new wxButton(this,wxID_OK,wxT("Update"));
	_resetButton = new wxButton(this, RESET_BUTTON, wxT("Reset Canvas"));
	_designButton = new wxButton(this, DESIGN_BUTTON, wxT("Activate Canvas"));

	wxStaticText* _instruct1Label = new wxStaticText(this, -1, wxT("Click the 'Activate Canvas' button to start drawing "));
	wxStaticText* _instruct2Label = new wxStaticText(this, -1, wxT("Points are selected by using the right mouse button "));
	wxStaticText* _circleLabel = new wxStaticText(this, -1, wxT("Circles are specified by two points: center and radius  "));
	wxStaticText* _rectLabel = new wxStaticText(this, -1, wxT("Rectangles are specified by opposite corners "));

	_designCanvas = new GL_Engine(this,-1,
                                wxPoint(0, 0),
                                wxSize(500, 500), 
                                wxSUNKEN_BORDER, 
                                wxT("OpenGL Design Canvas"));

	wxBoxSizer* _leftside = new wxBoxSizer(wxVERTICAL);
	_leftside->Add(_designButton, 1, wxALIGN_LEFT);
  _leftside->Add(_designCanvas, 12, wxALIGN_CENTER_HORIZONTAL);

	wxBoxSizer* _rightside = new wxBoxSizer(wxVERTICAL);
	_rightside->Add(_instruct1Label, 1, wxALIGN_LEFT);
	_rightside->Add(_instruct2Label, 1, wxALIGN_LEFT);
	_rightside->Add(_circleLabel, 1, wxALIGN_LEFT);
	_rightside->Add(_rectLabel, 1, wxALIGN_LEFT);
	_rightside->AddSpacer( 75 );
  _rightside->Add(_selgeomRBox, 3, wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
	_rightside->AddSpacer( 75 );
  _rightside->Add(_resetButton, 1, wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
	_rightside->AddSpacer( 75 );
  _rightside->Add(_updateButton, 1, wxALIGN_CENTER_HORIZONTAL|wxEXPAND);

	  wxBoxSizer* _mainSizer = new wxBoxSizer(wxHORIZONTAL);

  _mainSizer->Add(_leftside, 2, wxALIGN_LEFT);
  _mainSizer->Add(_rightside, 1, wxALIGN_RIGHT);

   //set this flag and let wx handle alignment
   SetAutoLayout(true);
   //assign the group to the panel
   SetSizer(_mainSizer);
   _mainSizer->Fit(this);
}

/////////////////////////////////////////////////////
GLCanvasSample_UI_Dialog
::~GLCanvasSample_UI_Dialog()
{
}

/////////////////////////////////////////////////////

bool GLCanvasSample_UI_Dialog::TransferDataFromWindow()
{
	(*p_type) = _selgeomRBox->GetSelection();
	(*p_radius) = _designCanvas->radius;
	(*p_length) = abs( _designCanvas->pt2[0] - _designCanvas->pt1[0] );
	(*p_width) = abs( _designCanvas->pt2[1] - _designCanvas->pt1[1] );

  return true;
}

////////////////////////////////////////////////////
bool GLCanvasSample_UI_Dialog::TransferDataToWindow()
{
    return true;
}

void GLCanvasSample_UI_Dialog::Lock(bool l)
{
}

void GLCanvasSample_UI_Dialog::_onGeomMethod(wxCommandEvent& event)
{

}

void GLCanvasSample_UI_Dialog::_onActCanvas(wxCommandEvent& event)
{
	_designCanvas->SetCurrent();
  _designCanvas->_draw();  
  _designCanvas->SwapBuffers();
	if ( _designCanvas->activepts == 1 )
	{
		_designCanvas->_drawPoint( _designCanvas->pt1[0], _designCanvas->pt1[1] );
	}
	else if ( _designCanvas->activepts == 2 )
	{
		_designCanvas->_drawPoint( _designCanvas->pt1[0], _designCanvas->pt1[1] );
		_designCanvas->_drawPoint( _designCanvas->pt2[0], _designCanvas->pt2[1] );
		_designCanvas->_drawShapes();
	}
}

void GLCanvasSample_UI_Dialog::_onResetCanvas(wxCommandEvent& event)
{
	_designCanvas->activepts = 0;
	_designCanvas->SetCurrent();
  _designCanvas->_draw();  
  _designCanvas->SwapBuffers();

	for ( int i=0; i<2; i++ )
	{
		_designCanvas->pt1[i] = 0;
		_designCanvas->pt2[i] = 0;
	}
	_designCanvas->radius = 0;

}
