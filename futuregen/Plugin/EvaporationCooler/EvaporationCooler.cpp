
#pragma warning(disable:4786)
#pragma warning(disable : 4101)
#pragma warning(disable : 4503)
#pragma warning(disable : 4251)

#include "EvaporationCooler.h"
#include "EvaporationCooler_UI.h"

IMPLEMENT_DYNAMIC_CLASS(EvaporationCooler, REI_Plugin)

/////////////////////////////////////////////////////////////////////////////
EvaporationCooler
::EvaporationCooler()
{
  RegistVar("desired_temp", &desired_temp);
  RegistVar("air_temp", &air_temp);
  RegistVar("air_humidity", &air_humidity);
  RegistVar("ambient_pres", &ambient_pres);
  
  desired_temp = 305;
  air_temp = 298;
  air_humidity = 20;
  ambient_pres = 101325;

  wxString icon_file="Icons/evap_cooler.gif";
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
EvaporationCooler
::~EvaporationCooler()
{

}

/////////////////////////////////////////////////////////////////////////////
double EvaporationCooler::GetVersion()
{
  double result=1.0;

  //Your code

  return result;
}

/////////////////////////////////////////////////////////////////////////////
int EvaporationCooler::GetNumPoly()
{
  int result=n_pts;
  //Your code
  return n_pts;
}

/////////////////////////////////////////////////////////////////////////////
//void EvaporationCooler::GetPoly(POLY &polygon)
//{
//  return ;//polygon;
//}

/////////////////////////////////////////////////////////////////////////////
int EvaporationCooler::GetNumIports()
{
  int result=1;

  return result;
}

/////////////////////////////////////////////////////////////////////////////
void EvaporationCooler::GetIPorts(POLY &iports)
{
  iports[0]=wxPoint(0,20);
  return;
}

/////////////////////////////////////////////////////////////////////////////
int EvaporationCooler::GetNumOports()
{
  int result=1;
  //Your code
  return result;
}

/////////////////////////////////////////////////////////////////////////////
void EvaporationCooler::GetOPorts(POLY &oports)
{
  oports[0]=wxPoint(40,20);
}

/////////////////////////////////////////////////////////////////////////////
void EvaporationCooler::DrawIcon(wxDC* dc)
{
  //Your implementation
  /*wxBrush old_brush=dc->GetBrush();
  dc->SetBrush(*wxBLACK_BRUSH);
  wxCoord xoff = pos.x;
  wxCoord yoff = pos.y;
  dc->DrawPolygon(n_pts, poly, xoff, yoff);
  dc->SetBrush(old_brush);*/
  dc->DrawBitmap(*my_icon,pos.x, pos.y);
}

/////////////////////////////////////////////////////////////////////////////
UIDialog* EvaporationCooler::UI(wxWindow* parent)
{
  if (dlg!=NULL)
    return dlg;
  
  dlg = new EvaporationCooler_UI_Dialog (parent, -1,
     &desired_temp,
     &air_temp,
     &air_humidity,
     &ambient_pres);
      
  return dlg;
}

/////////////////////////////////////////////////////////////////////////////
wxString EvaporationCooler::GetName()
{
  wxString result="REI_LarryRuth_EvaporationCooler"; //your name
  return result;
}

/////////////////////////////////////////////////////////////////////////////
wxString EvaporationCooler::GetDesc()
{
  wxString result="Evaporation Cooler Module by REI"; //your description

  return result;
}


