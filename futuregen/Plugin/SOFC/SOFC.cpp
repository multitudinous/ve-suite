
#pragma warning(disable:4786)
#pragma warning(disable : 4101)
#pragma warning(disable : 4503)
#pragma warning(disable : 4251)

#include "SOFC.h"
#include "SOFC_UI.h"

IMPLEMENT_DYNAMIC_CLASS(SOFC, REI_Plugin)

/////////////////////////////////////////////////////////////////////////////
SOFC
::SOFC()
{
  RegistVar("a_thickness", &a_thickness);
  RegistVar("c_thickness", &c_thickness);
  RegistVar("e_thickness", &e_thickness);
  RegistVar("a_A", &a_A);
  RegistVar("a_E", &a_E);
  RegistVar("c_A", &c_A);
  RegistVar("c_E", &c_E);
  RegistVar("e_A", &e_A);
  RegistVar("e_E", &e_E);
  RegistVar("cell_area", &cell_area);
  RegistVar("num_cell", &num_cell);
  RegistVar("fuel_util", &fuel_util);
  RegistVar("press_drop", &press_drop);

  a_thickness = 0.15;
  c_thickness = 0.01;
  e_thickness = 0.001;
  press_drop = 48263;
  a_A = 0.00298;
  a_E = -1392.0;
  c_A = 0.008144;
  c_E = 600.0;
  e_A = 0.00294;
  e_E = 10350.0;
  cell_area = 304.0;
  num_cell = 1658260;
  fuel_util = 85.0;

  wxString icon_file="Icons/sofc.gif";
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
SOFC
::~SOFC()
{

}

/////////////////////////////////////////////////////////////////////////////
double SOFC::GetVersion()
{
  double result=1.0;

  //Your code

  return result;
}

/////////////////////////////////////////////////////////////////////////////
int SOFC::GetNumPoly()
{
  int result=0;
  //Your code
  return n_pts;
}

/////////////////////////////////////////////////////////////////////////////
//void SOFC::GetPoly(POLY &polygon)
//{
//  return ;//polygon;
//}

/////////////////////////////////////////////////////////////////////////////
int SOFC::GetNumIports()
{
  int result=2;

  return result;
}

/////////////////////////////////////////////////////////////////////////////
void SOFC::GetIPorts(POLY &iports)
{
  iports[0]=wxPoint(0,20);
  iports[1]=wxPoint(20, 0);
  return;
}

/////////////////////////////////////////////////////////////////////////////
int SOFC::GetNumOports()
{
  int result=1;
  //Your code
  return result;
}

/////////////////////////////////////////////////////////////////////////////
void SOFC::GetOPorts(POLY &oports)
{
  oports[0]=wxPoint(40,20); 
}

/////////////////////////////////////////////////////////////////////////////
void SOFC::DrawIcon(wxDC* dc)
{
	dc->DrawBitmap(*my_icon,pos.x, pos.y);
	/*
  wxBrush old_brush=dc->GetBrush();
  dc->SetBrush(*wxGREEN_BRUSH);
  wxCoord xoff = pos.x;
  wxCoord yoff = pos.y;
  dc->DrawPolygon(n_pts, poly, xoff, yoff);
  dc->SetBrush(old_brush);
  //Your implementation*/
}

/////////////////////////////////////////////////////////////////////////////
UIDialog* SOFC::UI(wxWindow* parent)
{
  if (dlg!=NULL)
    return dlg;
  
  dlg = new SOFC_UI_Dialog (parent, -1,
     &a_thickness,
     &c_thickness,
     &e_thickness,
     &a_A,
     &a_E,
     &c_A,
     &c_E,
     &e_A,
     &e_E,
     &cell_area,
     &num_cell,
     &fuel_util,
     &press_drop);
      
  return dlg;
}

/////////////////////////////////////////////////////////////////////////////
wxString SOFC::GetName()
{
  wxString result="REI_LarryRuth_SOFC"; //your name
  return result;
}

/////////////////////////////////////////////////////////////////////////////
wxString SOFC::GetDesc()
{
  wxString result="SOFC Module by REI"; //your description

  return result;
}


