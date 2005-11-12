#include "GLCanvasSampleApp.h"
#include "GLCanvasSampleApp_UI_Dialog.h"
#include "sampleglcanvas.xpm"

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

  wxImage my_img( sampleglcanvas_xpm );
  icon_w = my_img.GetWidth();
  icon_h = my_img.GetHeight();
  my_icon=new wxBitmap(my_img.Scale(icon_w, icon_h));

  n_pts = 4;

  poly[0]=wxPoint(0,0);
  poly[1]=wxPoint(icon_w,0);
  poly[2]=wxPoint(icon_w,icon_h);
  poly[3]=wxPoint(0,icon_h);
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
  wxString result="Samples_GLCanvas"; //your name
  return result;
}

/////////////////////////////////////////////////////////////////////////////
wxString GLCanvasSampleApp::GetDesc()
{
  wxString result="Sample App using wxWidgets GLCanvas"; //your description

  return result;
}


