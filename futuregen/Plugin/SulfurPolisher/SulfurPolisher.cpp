
#pragma warning(disable:4786)
#pragma warning(disable : 4101)
#pragma warning(disable : 4503)
#pragma warning(disable : 4251)

#include "SulfurPolisher.h"
#include "SulfurPolisher_UI.h"

IMPLEMENT_DYNAMIC_CLASS(SulfurPolisher, REI_Plugin)

/////////////////////////////////////////////////////////////////////////////
SulfurPolisher
::SulfurPolisher()
{
  RegistVar("Temp_change", &Temp_change);
  RegistVar("H2S_eff", &H2S_eff);
  RegistVar("H2S_ppm", &H2S_ppm);
  RegistVar("COS_eff", &COS_eff);
  RegistVar("COS_ppm", &COS_ppm);
  RegistVar("Pres_drop", &Pres_drop);
  RegistVar("Bed_diameter", &Bed_diameter);
  RegistVar("Bed_depth", &Bed_depth);
  RegistVar("Bed_void_frac", &Bed_void_frac);
  RegistVar("Particle_size", &Particle_size);
  RegistVar("Particle_sphericity", &Particle_sphericity);
  RegistVar("rH2S_eff_ppm", &rH2S_eff_ppm);
  RegistVar("rCOS_eff_ppm", &rCOS_eff_ppm);
  RegistVar("rPresDrop_spec_calc", &rPresDrop_spec_calc);

  Temp_change = 0;
  H2S_eff = 0;
  H2S_ppm = 1;
  COS_eff = 0;
  COS_ppm = 1;
  Pres_drop = 5;
  Bed_diameter = 0;
  Bed_depth = 0;
  Bed_void_frac = 0;
  Particle_size = 0;
  Particle_sphericity = 0.65;
  rH2S_eff_ppm = 1;
  rCOS_eff_ppm = 1;
  rPresDrop_spec_calc = 0;

  wxString icon_file="Icons/polisher.gif";
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
SulfurPolisher
::~SulfurPolisher()
{

}

/////////////////////////////////////////////////////////////////////////////
double SulfurPolisher::GetVersion()
{
  double result=1.0;

  //Your code

  return result;
}

/////////////////////////////////////////////////////////////////////////////
int SulfurPolisher::GetNumPoly()
{
  int result=n_pts;
  //Your code
  return n_pts;
}

/////////////////////////////////////////////////////////////////////////////
//void SulfurPolisher::GetPoly(POLY &polygon)
//{
//  return ;//polygon;
//}

/////////////////////////////////////////////////////////////////////////////
int SulfurPolisher::GetNumIports()
{
  int result=1;

  return result;
}

/////////////////////////////////////////////////////////////////////////////
void SulfurPolisher::GetIPorts(POLY &iports)
{
  iports[0]=wxPoint(icon_w*24/48,icon_h*23/96);
  return;
}

/////////////////////////////////////////////////////////////////////////////
int SulfurPolisher::GetNumOports()
{
  int result=1;
  //Your code
  return result;
}

/////////////////////////////////////////////////////////////////////////////
void SulfurPolisher::GetOPorts(POLY &oports)
{
  oports[0]=wxPoint(icon_w*24/48, icon_h*78/96); 
}

/////////////////////////////////////////////////////////////////////////////
void SulfurPolisher::DrawIcon(wxDC* dc)
{
	/*
  wxBrush old_brush=dc->GetBrush();
  dc->SetBrush(*wxCYAN_BRUSH);
  wxCoord xoff = pos.x;
  wxCoord yoff = pos.y;
  dc->DrawPolygon(n_pts, poly, xoff, yoff);
  dc->SetBrush(old_brush);
  //Your implementation*/
	dc->DrawBitmap(*my_icon,pos.x, pos.y);
}

/////////////////////////////////////////////////////////////////////////////
UIDialog* SulfurPolisher::UI(wxWindow* parent)
{
  if (dlg!=NULL)
    return dlg;
  
  dlg = new SulfurPolisher_UI_Dialog (parent, -1,
     &Temp_change,
     &H2S_eff,
     &H2S_ppm,
     &COS_eff,
     &COS_ppm,
     &Pres_drop,
     &Bed_diameter,
     &Bed_depth,
     &Bed_void_frac,
     &Particle_size,
     &Particle_sphericity,
     &rH2S_eff_ppm,
     &rCOS_eff_ppm,
     &rPresDrop_spec_calc);
      
  return dlg;
}

/////////////////////////////////////////////////////////////////////////////
wxString SulfurPolisher::GetName()
{
  wxString result="REI_Components_SulfurPolisher"; //your name
  return result;
}

/////////////////////////////////////////////////////////////////////////////
wxString SulfurPolisher::GetDesc()
{
  wxString result="Sulfur Polisher Module by REI"; //your description

  return result;
}


