
#pragma warning(disable:4786)
#pragma warning(disable : 4101)
#pragma warning(disable : 4503)
#pragma warning(disable : 4251)

#include "AGR.h"
#include "AGR_UI.h"

IMPLEMENT_DYNAMIC_CLASS(AGR, REI_Plugin)

/////////////////////////////////////////////////////////////////////////////
AGR
::AGR()
{
  RegistVar("solv_mw", &solv_mw);
  RegistVar("solv_den", &solv_den);
  RegistVar("solv_type", &solv_type);
  RegistVar("tray_type", &tray_type);

  solv_mw = 119.16;
  solv_den = 1041.0;
  solv_type = 0;
  tray_type = 0;

  wxString icon_file="Icons/AGR.gif";
  wxImage my_img(icon_file, wxBITMAP_TYPE_GIF);
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
AGR
::~AGR()
{

}

/////////////////////////////////////////////////////////////////////////////
double AGR::GetVersion()
{
  double result=1.0;

  //Your code

  return result;
}

/////////////////////////////////////////////////////////////////////////////
int AGR::GetNumPoly()
{
  int result=0;
  //Your code
  return n_pts;
}

/////////////////////////////////////////////////////////////////////////////
//void AGR::GetPoly(POLY &polygon)
//{
//  return ;//polygon;
//}

/////////////////////////////////////////////////////////////////////////////
int AGR::GetNumIports()
{
  int result=1;

  return result;
}

/////////////////////////////////////////////////////////////////////////////
void AGR::GetIPorts(POLY &iports)
{
  iports[0]=wxPoint(icon_w*3/43,icon_h*21/41);
  return;
}

/////////////////////////////////////////////////////////////////////////////
int AGR::GetNumOports()
{
  int result=1;
  //Your code
  return result;
}

/////////////////////////////////////////////////////////////////////////////
void AGR::GetOPorts(POLY &oports)
{
  oports[0]=wxPoint(icon_w*40/43,icon_h*21/41);
}

/////////////////////////////////////////////////////////////////////////////
void AGR::DrawIcon(wxDC* dc)
{
  //Your implementation
  dc->DrawBitmap(*my_icon,pos.x, pos.y);
}

/////////////////////////////////////////////////////////////////////////////
UIDialog* AGR::UI(wxWindow* parent)
{
  if (dlg!=NULL)
    return dlg;
  
  dlg = new AGR_UI_Dialog(parent, -1,
     &solv_mw,
     &solv_den,
     &solv_type,
     &tray_type);
      
  return dlg;
}

/////////////////////////////////////////////////////////////////////////////
wxString AGR::GetName()
{
  wxString result="REI_Components_AGR"; //your name
  return result;
}

/////////////////////////////////////////////////////////////////////////////
wxString AGR::GetDesc()
{
  wxString result="AGR Module by REI"; //your description

  return result;
}

wxString AGR::GetHelp()
{
  wxString result="Framework/doc/modules/AGR.html"; //your description

  return result;
}
