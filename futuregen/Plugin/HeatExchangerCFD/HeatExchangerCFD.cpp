
#pragma warning(disable:4786)
#pragma warning(disable : 4101)
#pragma warning(disable : 4503)
#pragma warning(disable : 4251)

#include "HeatExchangerCFD.h"

IMPLEMENT_DYNAMIC_CLASS(HeatExchangerCFD, REI_Plugin)

/////////////////////////////////////////////////////////////////////////////
HeatExchangerCFD
::HeatExchangerCFD()
{
  wxString icon_file="Icons/heat_exchanger.gif";
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
HeatExchangerCFD
::~HeatExchangerCFD()
{

}

/////////////////////////////////////////////////////////////////////////////
double HeatExchangerCFD::GetVersion()
{
  double result=1.0;

  //Your code

  return result;
}

/////////////////////////////////////////////////////////////////////////////
int HeatExchangerCFD::GetNumPoly()
{
  int result=0;
  //Your code
  return n_pts;
}

/////////////////////////////////////////////////////////////////////////////
//void HeatExchangerCFD::GetPoly(POLY &polygon)
//{
//  return ;//polygon;
//}

/////////////////////////////////////////////////////////////////////////////
int HeatExchangerCFD::GetNumIports()
{
  int result=1;

  return result;
}

/////////////////////////////////////////////////////////////////////////////
void HeatExchangerCFD::GetIPorts(POLY &iports)
{
  iports[0]=wxPoint(icon_w*6/60,icon_h*14/45);
  return;
}

/////////////////////////////////////////////////////////////////////////////
int HeatExchangerCFD::GetNumOports()
{
  int result=1;
  //Your code
  return result;
}

/////////////////////////////////////////////////////////////////////////////
void HeatExchangerCFD::GetOPorts(POLY &oports)
{
  oports[0]=wxPoint(icon_w*52/60, icon_h*32/45);
}

/////////////////////////////////////////////////////////////////////////////
void HeatExchangerCFD::DrawIcon(wxDC* dc)
{
  //Your implementation
  dc->DrawBitmap(*my_icon,pos.x, pos.y);
}

/////////////////////////////////////////////////////////////////////////////
UIDialog* HeatExchangerCFD::UI(wxWindow* parent)
{
  return NULL;
}

/////////////////////////////////////////////////////////////////////////////
wxString HeatExchangerCFD::GetName()
{
  wxString result="REI_Components_HeatExchangerCFD"; //your name
  return result;
}

/////////////////////////////////////////////////////////////////////////////
wxString HeatExchangerCFD::GetDesc()
{
  wxString result="HeatExchangerCFD Module by REI"; //your description

  return result;
}

wxString HeatExchangerCFD::GetHelp()
{
  wxString result="Framework/doc/modules/HeatExchangerCFD.html"; //your description
  return result;
}

bool HeatExchangerCFD::Has3Ddata()
{
  return true;
}