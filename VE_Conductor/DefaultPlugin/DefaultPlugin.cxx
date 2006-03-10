#include "VE_Conductor/DefaultPlugin/DefaultPlugin.h"
#include "VE_Conductor/DefaultPlugin/DefaultPlugin_UI_Dialog.h"

#include <wx/dc.h>
IMPLEMENT_DYNAMIC_CLASS(DefaultPlugin, REI_Plugin)

/////////////////////////////////////////////////////////////////////////////
DefaultPlugin
::DefaultPlugin()
{
  wxString icon_file="Icons/Armored.jpg";
  wxImage my_img(icon_file, wxBITMAP_TYPE_JPEG);
  icon_w = (int)my_img.GetWidth()*0.20;
  icon_h = (int)my_img.GetHeight()*0.20;
  my_icon=new wxBitmap(my_img.Scale(icon_w, icon_h));

  n_pts = 4;

  poly[0]=wxPoint(0,0);
  poly[1]=wxPoint(icon_w,0);
  poly[2]=wxPoint(icon_w,icon_h);
  poly[3]=wxPoint(0,icon_h);

}



/////////////////////////////////////////////////////////////////////////////
DefaultPlugin
::~DefaultPlugin()
{

}

/////////////////////////////////////////////////////////////////////////////
double DefaultPlugin::GetVersion()
{
  double result=1.0;

  //Your code

  return result;
}

/////////////////////////////////////////////////////////////////////////////
int DefaultPlugin::GetNumPoly()
{
  int result=0;
  //Your code
  return n_pts;
}
/*
/////////////////////////////////////////////////////////////////////////////
void DefaultPlugin::GetPoly(POLY &polygon)
{
  return ;//polygon;
}
*/
/////////////////////////////////////////////////////////////////////////////
int DefaultPlugin::GetNumIports()
{
  int result=1;
  
  return result;
}

/////////////////////////////////////////////////////////////////////////////
void DefaultPlugin::GetIPorts(POLY &iports)
{
  iports[0] = wxPoint( icon_w*0.0, icon_h*0.50);
  
  return;
}

/////////////////////////////////////////////////////////////////////////////
int DefaultPlugin::GetNumOports()
{
  int result=0;
  //Your code
  return result;
}

/////////////////////////////////////////////////////////////////////////////
void DefaultPlugin::GetOPorts(POLY &oports)
{
  //oports[0]=wxPoint(icon_w*43/52,icon_h*74/98);     
}

/////////////////////////////////////////////////////////////////////////////
void DefaultPlugin::DrawIcon(wxDC* dc)
{
  //Your implementation
	dc->DrawBitmap(*my_icon,pos.x, pos.y);
}

/////////////////////////////////////////////////////////////////////////////
UIDialog* DefaultPlugin::UI(wxWindow* parent)
{
  if (dlg!=NULL)
    return dlg;
  
  dlg = new DefaultPlugin_UI_Dialog(parent, -1);
      
  return dlg;
}

/////////////////////////////////////////////////////////////////////////////
wxString DefaultPlugin::GetName()
{
  wxString result="VE_Conductor_Default_DefaultPlugin"; //your name
  return result;
}

/////////////////////////////////////////////////////////////////////////////
wxString DefaultPlugin::GetDesc()
{
  wxString result="DefaultPlugin for VE-Conductor."; //your description

  return result;
}


