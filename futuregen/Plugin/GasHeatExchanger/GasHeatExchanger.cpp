
#pragma warning(disable:4786)
#pragma warning(disable : 4101)
#pragma warning(disable : 4503)
#pragma warning(disable : 4251)

#include "GasHeatExchanger.h"
#include "GasHeatExchanger_UI.h"

IMPLEMENT_DYNAMIC_CLASS(GasHeatExchanger, REI_Plugin)

/////////////////////////////////////////////////////////////////////////////
GasHeatExchanger
::GasHeatExchanger()
{
  RegistVar("desired_temp", &desired_temp);

  desired_temp = 1000;
  
  wxString icon_file="Icons/heat_exchanger_gas.gif";
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
GasHeatExchanger
::~GasHeatExchanger()
{

}

/////////////////////////////////////////////////////////////////////////////
double GasHeatExchanger::GetVersion()
{
  double result=1.0;

  //Your code

  return result;
}

/////////////////////////////////////////////////////////////////////////////
int GasHeatExchanger::GetNumPoly()
{
  int result=0;
  //Your code
  return n_pts;
}

/////////////////////////////////////////////////////////////////////////////
//void GasHeatExchanger::GetPoly(POLY &polygon)
//{
//  return ;//polygon;
//}

/////////////////////////////////////////////////////////////////////////////
int GasHeatExchanger::GetNumIports()
{
  int result=1;

  return result;
}

/////////////////////////////////////////////////////////////////////////////
void GasHeatExchanger::GetIPorts(POLY &iports)
{
  iports[0]=wxPoint(icon_w*6/35, icon_h*18/34);
  return;
}

/////////////////////////////////////////////////////////////////////////////
int GasHeatExchanger::GetNumOports()
{
  int result=1;
  //Your code
  return result;
}

/////////////////////////////////////////////////////////////////////////////
void GasHeatExchanger::GetOPorts(POLY &oports)
{
  oports[0]=wxPoint(icon_w*28/35, icon_h*18/34);
}

/////////////////////////////////////////////////////////////////////////////
void GasHeatExchanger::DrawIcon(wxDC* dc)
{
  //Your implementation
/*
  wxCoord xoff = pos.x;
  wxCoord yoff = pos.y;
  
  dc->DrawPolygon(n_pts, poly, xoff, yoff);*/
  dc->DrawBitmap(*my_icon,pos.x, pos.y);
}

/////////////////////////////////////////////////////////////////////////////
UIDialog* GasHeatExchanger::UI(wxWindow* parent)
{
  if (dlg!=NULL)
    return dlg;
  
  dlg = new GasHeatExchanger_UI_Dialog (parent, -1,
     &desired_temp);
  
  return dlg;
}

/////////////////////////////////////////////////////////////////////////////
wxString GasHeatExchanger::GetName()
{
  wxString result="REI_Gas_GasHeatExchanger"; //your name
  return result;
}

/////////////////////////////////////////////////////////////////////////////
wxString GasHeatExchanger::GetDesc()
{
  wxString result="GasHeatExchanger Module by REI"; //your description

  return result;
}

wxString GasHeatExchanger::GetHelp()
{
  wxString result="Framework/doc/modules/GasHeatExchanger.html"; //your description

  return result;
}

