#pragma warning(disable : 4786)
#pragma warning(disable : 4101)
#pragma warning(disable : 4503)
#pragma warning(disable : 4251)


#include "SELX.h"
#include "SELX_UI.h"

IMPLEMENT_DYNAMIC_CLASS(SELX, REI_Plugin)

SELX::SELX()
{

  selx_idx_in_nspares = 0;
  co2_removal_d = 90.0;

  RegistVar("selx_idx_in_nspares", &selx_idx_in_nspares);
  RegistVar("co2_removal_d", &co2_removal_d);
  
  //  poly[0]=wxPoint(0,0);
  // poly[1]=wxPoint(40,0);
  //poly[2]=wxPoint(40,60);
  //poly[3]=wxPoint(20,80);
  //poly[4]=wxPoint(0,60);
  if (poly!=NULL)
    delete poly;
  
  wxString selx_icon_file="selexol.jpg";
  wxImage my_img(selx_icon_file, wxBITMAP_TYPE_JPEG);
  icon_w = my_img.GetWidth() / 5;
  icon_h = my_img.GetHeight() / 5;
  my_icon=new wxBitmap(my_img.Scale(icon_w, icon_h));

  poly = new wxPoint[4];
  n_pts = 4;

  poly[0]=wxPoint(0,0);
  poly[1]=wxPoint(icon_w,0);
  poly[2]=wxPoint(icon_w,icon_h);
  poly[3]=wxPoint(0,icon_h);
}

wxString SELX::GetName()
{
  return _T("CMU_IECM_CO2-Capture");
}

wxString SELX::GetDesc()
{
  return _T("CO2 Capture Facility");
}

UIDialog* SELX::UI(wxWindow* parent)
{
  if (dlg!=NULL)
    return dlg;
  
  dlg = new SELX_UI_Dialog(parent, -1, &selx_idx_in_nspares, &co2_removal_d);

  return dlg;
}

void SELX::GetIPorts(POLY &iports)
{
  iports[0]=wxPoint(icon_w/3-1, icon_h/2);
}

void SELX::GetOPorts(POLY &oports)
{

  oports[0] = wxPoint(icon_w*2/3+2, icon_h/2);
    
}

void SELX::DrawIcon(wxDC* dc)
{
   dc->DrawBitmap(*my_icon,pos.x, pos.y);
  /*
  wxBrush old_brush=dc->GetBrush();
  wxCoord xoff = pos.x;
  wxCoord yoff = pos.y;
  wxColour* color;
  color = wxTheColourDatabase->FindColour("MEDIUM SPRING GREEN");
  wxBrush mybrush((*color), wxSOLID);
  dc->SetBrush(mybrush);

  dc->DrawPolygon(n_pts, poly, xoff, yoff);
  
  dc->SetBrush(old_brush);
  */
}
