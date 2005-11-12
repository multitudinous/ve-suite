#include "GLCanvasSampleApp_UI_Dialog.h"
#include <math.h>

BEGIN_EVENT_TABLE(GLCanvasSampleApp_UI_Dialog, UIDialog)
	EVT_BUTTON		(DESIGN_BUTTON,				GLCanvasSampleApp_UI_Dialog::_onActCanvas)
   EVT_RADIOBOX	(GEOM_SHAPE_RADIOBOX,		GLCanvasSampleApp_UI_Dialog::_onGeomMethod)
	EVT_BUTTON		(RESET_BUTTON,             GLCanvasSampleApp_UI_Dialog::_onResetCanvas)
END_EVENT_TABLE()

//Here is the constructor with passed in pointers
GLCanvasSampleApp_UI_Dialog
::GLCanvasSampleApp_UI_Dialog
(wxWindow* parent, int id,
  double* radius,
  double* length,
  double* width,
  double* xcoord,
  double* ycoord,
  long* type)
: UIDialog((wxWindow *) parent, id, "GLCanvasSampleApp"),
  p_radius(radius),
  p_length(length),
  p_width(width),
  p_xcoord(xcoord),
  p_ycoord(ycoord),
  p_type(type)
{
   (*p_radius) = 0;
	(*p_length) = 0;
	(*p_width) = 0;
	(*p_type) = 0;
   (*p_xcoord) = 0;
   (*p_ycoord) = 0;

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
GLCanvasSampleApp_UI_Dialog
::~GLCanvasSampleApp_UI_Dialog()
{
}

/////////////////////////////////////////////////////

bool GLCanvasSampleApp_UI_Dialog::TransferDataFromWindow()
{
  	(*p_type) = _selgeomRBox->GetSelection();
	(*p_radius) = _designCanvas->radius;
	(*p_length) = abs( _designCanvas->pt2[0] - _designCanvas->pt1[0] );
	(*p_width) = abs( _designCanvas->pt2[1] - _designCanvas->pt1[1] );

  return true;
}

////////////////////////////////////////////////////
bool GLCanvasSampleApp_UI_Dialog::TransferDataToWindow()
{
    return true;
}

void GLCanvasSampleApp_UI_Dialog::Lock(bool l)
{
}

void GLCanvasSampleApp_UI_Dialog::_onGeomMethod(wxCommandEvent& event)
{

}

void GLCanvasSampleApp_UI_Dialog::_onActCanvas(wxCommandEvent& event)
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

void GLCanvasSampleApp_UI_Dialog::_onResetCanvas(wxCommandEvent& event)
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
