
#pragma warning(disable:4786)
#pragma warning(disable : 4101)
#pragma warning(disable : 4503)
#pragma warning(disable : 4251)

#include "ChlorineBed.h"
#include "ChlorineBed_UI.h"

IMPLEMENT_DYNAMIC_CLASS(ChlorineBed, REI_Plugin)

/////////////////////////////////////////////////////////////////////////////
ChlorineBed
::ChlorineBed()
{
  RegistVar("Temp_change", &Temp_change);
  RegistVar("HCL_eff", &HCL_eff);
  RegistVar("HCL_ppm", &HCL_ppm);
  RegistVar("Pres_drop", &Pres_drop);
  RegistVar("Bed_diameter", &Bed_diameter);
  RegistVar("Bed_depth", &Bed_depth);
  RegistVar("Bed_void_frac", &Bed_void_frac);
  RegistVar("Particle_size", &Particle_size);
  RegistVar("Particle_sphericity", &Particle_sphericity);
  RegistVar("rHCL_eff_ppm", &rHCL_eff_ppm);
  RegistVar("rPresDrop_spec_calc", &rPresDrop_spec_calc);

  Temp_change = -5;
  HCL_eff = 0;
  HCL_ppm = 1;
  Pres_drop = 5;
  Bed_diameter = 0;
  Bed_depth = 0;
  Bed_void_frac = 0;
  Particle_size = 0;
  Particle_sphericity = 0.65;
  rHCL_eff_ppm = 1;
  rPresDrop_spec_calc = 0;
  
  wxString icon_file="Icons/chlorinebed.gif";
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
ChlorineBed
::~ChlorineBed()
{

}

/////////////////////////////////////////////////////////////////////////////
double ChlorineBed::GetVersion()
{
  double result=1.0;

  //Your code

  return result;
}

/////////////////////////////////////////////////////////////////////////////
int ChlorineBed::GetNumPoly()
{
  int result=n_pts;
  //Your code
  return n_pts;
}

/////////////////////////////////////////////////////////////////////////////
//void ChlorineBed::GetPoly(POLY &polygon)
//{
//  return ;//polygon;
//}

/////////////////////////////////////////////////////////////////////////////
int ChlorineBed::GetNumIports()
{
  int result=1;

  return result;
}

/////////////////////////////////////////////////////////////////////////////
void ChlorineBed::GetIPorts(POLY &iports)
{
  iports[0]=wxPoint(icon_w*19/37, icon_h*18/98);
  return;
}

/////////////////////////////////////////////////////////////////////////////
int ChlorineBed::GetNumOports()
{
  int result=1;
  //Your code
  return result;
}

/////////////////////////////////////////////////////////////////////////////
void ChlorineBed::GetOPorts(POLY &oports)
{
  oports[0]=wxPoint(icon_w*19/37, icon_h*82/98);
}

/////////////////////////////////////////////////////////////////////////////
void ChlorineBed::DrawIcon(wxDC* dc)
{
  dc->DrawBitmap(*my_icon,pos.x, pos.y);
	/*
  wxBrush old_brush=dc->GetBrush();
  dc->SetBrush(*wxBLUE_BRUSH);
  wxCoord xoff = pos.x;
  wxCoord yoff = pos.y;
  dc->DrawPolygon(n_pts, poly, xoff, yoff);
  dc->SetBrush(old_brush);*/
  //Your implementation
}

/////////////////////////////////////////////////////////////////////////////
UIDialog* ChlorineBed::UI(wxWindow* parent)
{
  if (dlg!=NULL)
    return dlg;
  
  dlg = new ChlorineBed_UI_Dialog (parent, -1,
     &Temp_change,
     &HCL_eff,
     &HCL_ppm,
     &Pres_drop,
     &Bed_diameter,
     &Bed_depth,
     &Bed_void_frac,
     &Particle_size,
     &Particle_sphericity,
     &rHCL_eff_ppm,
     &rPresDrop_spec_calc);
      
  return dlg;
}

/////////////////////////////////////////////////////////////////////////////
wxString ChlorineBed::GetName()
{
  wxString result="REI_Components_ChlorineBed"; //your name
  return result;
}

/////////////////////////////////////////////////////////////////////////////
wxString ChlorineBed::GetDesc()
{
  wxString result="Chlorine Bed Module by REI"; //your description

  return result;
}


