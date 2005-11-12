#include "GLCanvasSampleApp.h"
#include "GLCanvasSampleApp_UI_Dialog.h"

IMPLEMENT_DYNAMIC_CLASS(GLCanvasSampleApp, REI_Plugin)

/////////////////////////////////////////////////////////////////////////////
GLCanvasSampleApp
::GLCanvasSampleApp()
{
  RegistVar("radius", &radius);
  radius = 0.0f;
  RegistVar("length", &length);
  length = 0.0f;
  RegistVar("width", &width);
  width = 0.0f;
  RegistVar("xcoord", &xcoord);
  xcoord = 0.0f;
  RegistVar("ycoord", &ycoord);
  ycoord = 0.0f;
  RegistVar("type", &type);
  type = 0;
}



/////////////////////////////////////////////////////////////////////////////
GLCanvasSampleApp
::~GLCanvasSampleApp()
{

}

/////////////////////////////////////////////////////////////////////////////
double GLCanvasSampleApp::GetVersion()
{
  double result=1.0;

  //Your code

  return result;
}

/////////////////////////////////////////////////////////////////////////////
int GLCanvasSampleApp::GetNumPoly()
{
  int result=0;
  //Your code
  return n_pts;
}

/////////////////////////////////////////////////////////////////////////////
int GLCanvasSampleApp::GetNumIports()
{
  int result=0;

  return result;
}

/////////////////////////////////////////////////////////////////////////////
void GLCanvasSampleApp::GetIPorts(POLY &iports)
{
  
  return;
}

/////////////////////////////////////////////////////////////////////////////
int GLCanvasSampleApp::GetNumOports()
{
  int result=0;
  //Your code
  return result;
}

/////////////////////////////////////////////////////////////////////////////
void GLCanvasSampleApp::GetOPorts(POLY &oports)
{
    
}

/////////////////////////////////////////////////////////////////////////////
void GLCanvasSampleApp::DrawIcon(wxDC* dc)
{
  //Your implementation
}

/////////////////////////////////////////////////////////////////////////////
UIDialog* GLCanvasSampleApp::UI(wxWindow* parent)
{
  if (dlg!=NULL)
    return dlg;
  
  dlg = new GLCanvasSampleApp_UI_Dialog(parent, -1,
     &radius,
     &length,
     &width,
     &xcoord,
     &ycoord,
     &type);
      
  return dlg;
}

/////////////////////////////////////////////////////////////////////////////
wxString GLCanvasSampleApp::GetName()
{
  wxString result="NEW_MOD"; //your name
  return result;
}

/////////////////////////////////////////////////////////////////////////////
wxString GLCanvasSampleApp::GetDesc()
{
  wxString result="None"; //your description

  return result;
}


