
#pragma warning(disable:4786)
#pragma warning(disable : 4101)
#pragma warning(disable : 4503)
#pragma warning(disable : 4251)

#include "Compressor.h"
#include "Compressor_UI.h"

IMPLEMENT_DYNAMIC_CLASS(Compressor, REI_Plugin)

/////////////////////////////////////////////////////////////////////////////
Compressor
::Compressor()
{
  RegistVar("eff", &eff);
  RegistVar("pressure_out", &pressure_out);
  RegistVar("pressure_change", &pressure_change);
  RegistVar("case_type", &case_type);
  
  eff = 0.82;
  pressure_out = 0.0;
  pressure_change = 2067854;
  case_type = 0;

  wxString icon_file="Icons/compressor.gif";
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
Compressor
::~Compressor()
{

}

/////////////////////////////////////////////////////////////////////////////
double Compressor::GetVersion()
{
  double result=1.0;

  //Your code

  return result;
}

/////////////////////////////////////////////////////////////////////////////
int Compressor::GetNumPoly()
{
  int result=0;
  //Your code
  return n_pts;
}

/////////////////////////////////////////////////////////////////////////////
//void Compressor::GetPoly(POLY &polygon)
//{
//  return ;//polygon;
//}

/////////////////////////////////////////////////////////////////////////////
int Compressor::GetNumIports()
{
  int result=1;

  return result;
}

/////////////////////////////////////////////////////////////////////////////
void Compressor::GetIPorts(POLY &iports)
{
  iports[0]=wxPoint(icon_w/5,0);
  return;
}

/////////////////////////////////////////////////////////////////////////////
int Compressor::GetNumOports()
{
  int result=1;
  //Your code
  return result;
}

/////////////////////////////////////////////////////////////////////////////
void Compressor::GetOPorts(POLY &oports)
{
  oports[0]=wxPoint(6*icon_w/7,icon_h); 
}

/////////////////////////////////////////////////////////////////////////////
void Compressor::DrawIcon(wxDC* dc)
{
  //Your implementation
	dc->DrawBitmap(*my_icon,pos.x, pos.y);
	/*
  wxBrush old_brush=dc->GetBrush();
  dc->SetBrush(*wxGREY_BRUSH);
  wxCoord xoff = pos.x;
  wxCoord yoff = pos.y;
  dc->DrawPolygon(n_pts, poly, xoff, yoff);
  dc->SetBrush(old_brush);
  */
}

/////////////////////////////////////////////////////////////////////////////
UIDialog* Compressor::UI(wxWindow* parent)
{
  if (dlg!=NULL)
    return dlg;
  
  dlg = new Compressor_UI_Dialog (parent, -1,
     &eff,
     &pressure_out,
     &pressure_change,
     &case_type);
      
  return dlg;
}

/////////////////////////////////////////////////////////////////////////////
wxString Compressor::GetName()
{
  wxString result="REI_LarryRuth_Compressor"; //your name
  return result;
}

/////////////////////////////////////////////////////////////////////////////
wxString Compressor::GetDesc()
{
  wxString result="Compressor/Expander Module by REI"; //your description

  return result;
}


