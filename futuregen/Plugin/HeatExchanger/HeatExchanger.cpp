
#pragma warning(disable:4786)
#pragma warning(disable : 4101)
#pragma warning(disable : 4503)
#pragma warning(disable : 4251)

#include "HeatExchanger.h"
#include "HeatExchanger_UI.h"

IMPLEMENT_DYNAMIC_CLASS(HeatExchanger, REI_Plugin)

/////////////////////////////////////////////////////////////////////////////
HeatExchanger
::HeatExchanger()
{
  RegistVar("Sl", &Sl);
  RegistVar("St", &St);
  RegistVar("tube_id", &tube_id);
  RegistVar("tube_od", &tube_od);
  RegistVar("tube_length", &tube_length);
  RegistVar("int_press_drop", &int_press_drop);
  RegistVar("ext_press_drop", &ext_press_drop);
  RegistVar("fin_effect", &fin_effect);
  RegistVar("arrangement", &arrangement);
  RegistVar("tube_config", &tube_config);
  RegistVar("num_tubeL", &num_tubeL);
  RegistVar("num_tubeX", &num_tubeX);
  RegistVar("use_fins", &use_fins);

  arrangement = "Shell & Tube";
  tube_config = "Inline";
  num_tubeL = 142;
  num_tubeX = 33;
  Sl = 0.127;
  St = 0.0826;
  tube_id = 0.0381;
  tube_od = 0.0508;
  tube_length = 11.948;
  int_press_drop = 0;
  ext_press_drop = 0;
  use_fins = 1;
  fin_effect = 2.0;

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
HeatExchanger
::~HeatExchanger()
{

}

/////////////////////////////////////////////////////////////////////////////
double HeatExchanger::GetVersion()
{
  double result=1.0;

  //Your code

  return result;
}

/////////////////////////////////////////////////////////////////////////////
int HeatExchanger::GetNumPoly()
{
  int result=0;
  //Your code
  return n_pts;
}

/////////////////////////////////////////////////////////////////////////////
//void HeatExchanger::GetPoly(POLY &polygon)
//{
//  return ;//polygon;
//}

/////////////////////////////////////////////////////////////////////////////
int HeatExchanger::GetNumIports()
{
  int result=2;

  return result;
}

/////////////////////////////////////////////////////////////////////////////
void HeatExchanger::GetIPorts(POLY &iports)
{
  iports[0]=wxPoint(icon_w*11/100,icon_h*23/75);
  iports[1]=wxPoint(icon_w*11/100,icon_h*53/75);
  return;
}

/////////////////////////////////////////////////////////////////////////////
int HeatExchanger::GetNumOports()
{
  int result=2;
  //Your code
  return result;
}

/////////////////////////////////////////////////////////////////////////////
void HeatExchanger::GetOPorts(POLY &oports)
{
  oports[0]=wxPoint(icon_w*87/100,icon_h*23/75);
  oports[1]=wxPoint(icon_w*87/100, icon_h*53/75);
}

/////////////////////////////////////////////////////////////////////////////
void HeatExchanger::DrawIcon(wxDC* dc)
{
  //Your implementation
	/*
  wxBrush old_brush=dc->GetBrush();
  dc->SetBrush(*wxLIGHT_GREY_BRUSH);
  wxCoord xoff = pos.x;
  wxCoord yoff = pos.y;
  dc->DrawPolygon(n_pts, poly, xoff, yoff);
  dc->SetBrush(old_brush);*/
  dc->DrawBitmap(*my_icon,pos.x, pos.y);
}

/////////////////////////////////////////////////////////////////////////////
UIDialog* HeatExchanger::UI(wxWindow* parent)
{
  if (dlg!=NULL)
    return dlg;
  
  dlg = new HeatExchanger_UI_Dialog (parent, -1,
     &Sl,
     &St,
     &tube_id,
     &tube_od,
     &tube_length,
     &int_press_drop,
     &ext_press_drop,
     &fin_effect,
     &arrangement,
     &tube_config,
     &num_tubeL,
     &num_tubeX,
     &use_fins);
      
  return dlg;
}

/////////////////////////////////////////////////////////////////////////////
wxString HeatExchanger::GetName()
{
  wxString result="REI_Components_HeatExchanger"; //your name
  return result;
}

/////////////////////////////////////////////////////////////////////////////
wxString HeatExchanger::GetDesc()
{
  wxString result="Heat Exchanger Module by REI"; //your description

  return result;
}


