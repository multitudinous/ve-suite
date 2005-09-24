#include "GLCanvasSample_UI_Dialog.h"

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
}

/////////////////////////////////////////////////////
GLCanvasSample_UI_Dialog
::~GLCanvasSample_UI_Dialog()
{
}

/////////////////////////////////////////////////////

bool GLCanvasSample_UI_Dialog::TransferDataFromWindow()
{
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

