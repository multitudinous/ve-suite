#ifndef GLCanvasSampleApp_UI_DIALOG_H
#define GLCanvasSampleApp_UI_DIALOG_H
#include "VE_Conductor/Framework/UIDialog.h"
#include "GL_Engine.h"
#include <wx/glcanvas.h>
#include <gl/glut.h>
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

