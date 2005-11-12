#include "GLCanvasSampleApp_UI_Dialog.h"

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
}

/////////////////////////////////////////////////////
GLCanvasSampleApp_UI_Dialog
::~GLCanvasSampleApp_UI_Dialog()
{
}

/////////////////////////////////////////////////////

bool GLCanvasSampleApp_UI_Dialog::TransferDataFromWindow()
{
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

