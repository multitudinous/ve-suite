
#ifdef WIN32
#pragma warning(disable : 4786)
#pragma warning(disable : 4101)
#pragma warning(disable : 4503)
#pragma warning(disable : 4251)
#endif

#include "GLCanvasSample.h"
#include "GLCanvasSample_UI_Dialog.h"

IMPLEMENT_DYNAMIC_CLASS(GLCanvasSample, REI_Plugin)

/////////////////////////////////////////////////////////////////////////////
GLCanvasSample
::GLCanvasSample()
{
  RegistVar("radius", &radius);
  RegistVar("length", &length);
  RegistVar("width", &width);
  RegistVar("type", &type);
}



/////////////////////////////////////////////////////////////////////////////
GLCanvasSample
::~GLCanvasSample()
{

}

/////////////////////////////////////////////////////////////////////////////
double GLCanvasSample::GetVersion()
{
  double result=1.0;

  //Your code

  return result;
}

/////////////////////////////////////////////////////////////////////////////
int GLCanvasSample::GetNumPoly()
{
  int result=0;
  //Your code
  return n_pts;
}

/////////////////////////////////////////////////////////////////////////////
int GLCanvasSample::GetNumIports()
{
  int result=0;

  return result;
}

/////////////////////////////////////////////////////////////////////////////
void GLCanvasSample::GetIPorts(POLY &iports)
{
  
  return;
}

/////////////////////////////////////////////////////////////////////////////
int GLCanvasSample::GetNumOports()
{
  int result=0;
  //Your code
  return result;
}

/////////////////////////////////////////////////////////////////////////////
void GLCanvasSample::GetOPorts(POLY &oports)
{
    
}

/////////////////////////////////////////////////////////////////////////////
void GLCanvasSample::DrawIcon(wxDC* dc)
{
  //Your implementation
}

/////////////////////////////////////////////////////////////////////////////
UIDialog* GLCanvasSample::UI(wxWindow* parent)
{
  if (dlg!=NULL)
    return dlg;
  
  dlg = new GLCanvasSample_UI_Dialog(parent, -1,
     &radius,
     &length,
     &width,
     &type);
      
  return dlg;
}

/////////////////////////////////////////////////////////////////////////////
wxString GLCanvasSample::GetName()
{
  wxString result="NEW_MOD"; //your name
  return result;
}

/////////////////////////////////////////////////////////////////////////////
wxString GLCanvasSample::GetDesc()
{
  wxString result="None"; //your description

  return result;
}


