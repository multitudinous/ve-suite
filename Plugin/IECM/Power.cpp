#pragma warning(disable:4786)
#pragma warning(disable : 4101)
#pragma warning(disable : 4503)
#pragma warning(disable : 4251)


#include "Power.h"
#include "Power_UI.h"

IMPLEMENT_DYNAMIC_CLASS(Power, REI_Plugin)

Power::Power()
{
  gts_idx_in_nturb = 2;
  RegistVar("gts_idx_in_nturb", &gts_idx_in_nturb);
 
  gts_idx_in_pres_ratio = 15.70;
  RegistVar("gts_idx_in_pres_ratio", &gts_idx_in_pres_ratio);

  gts_idx_in_heat_rate = 9000;
  RegistVar("gts_idx_in_heat_rate", &gts_idx_in_heat_rate);

  gts_idx_in_sox_so3 = 0.0;
  RegistVar("gts_idx_in_sox_so3", &gts_idx_in_sox_so3);

  gts_idx_in_nox_cons = 9.0;
  RegistVar("gts_idx_in_nox_cons",  &gts_idx_in_nox_cons);

  gts_idx_in_nox_no = 95.0;
  RegistVar("gts_idx_in_nox_no", &gts_idx_in_nox_no);

  gts_idx_in_carbon_co = 0.0;
  RegistVar("gts_idx_in_carbon_co", &gts_idx_in_carbon_co);

  if (poly!=NULL)
    delete poly;
  /*
  poly = new wxPoint[9];
  n_pts = 9;
  
  poly[0]=wxPoint(100,0);
  poly[1]=wxPoint(200,0);
  poly[2]=wxPoint(200,160);
  poly[3]=wxPoint(120,160);
  poly[4]=wxPoint(90, 140);
  poly[5]=wxPoint(30, 140);
  poly[6]=wxPoint(0,160);
  poly[7]=wxPoint(0,110);
  poly[8]=wxPoint(100, 110);
  
  */
  wxString power_icon_file="turbineWsupport.jpg";
  wxImage my_img(power_icon_file, wxBITMAP_TYPE_JPEG);
  icon_w = my_img.GetWidth();
  icon_h = my_img.GetHeight();
  my_icon=new wxBitmap(my_img.Scale(icon_w, icon_h));
  
  poly = new wxPoint[4];
  n_pts = 4;
  
  poly[0]=wxPoint(0,0);
  poly[1]=wxPoint(icon_w,0);
  poly[2]=wxPoint(icon_w,icon_h);
  poly[3]=wxPoint(0,icon_h);
  
}

wxString Power::GetName()
{
  return _T("CMU_IECM_PowerBlock");
}

wxString Power::GetDesc()
{
  return _T("Power Block. Including Gas Turbine, Steam Cycle and Emission control");
}

UIDialog* Power::UI(wxWindow* parent)
{
  if (dlg!=NULL)
    return dlg;
 
  dlg = new POWER_UI_Dialog(parent, -1, 
			    &gts_idx_in_nturb, 
			    &gts_idx_in_pres_ratio,
			    &gts_idx_in_heat_rate,
			    &gts_idx_in_sox_so3,
			    &gts_idx_in_nox_cons,
			    &gts_idx_in_nox_no,
			    &gts_idx_in_carbon_co);

  return dlg;
}

void Power::DrawIcon(wxDC* dc)
{
  dc->DrawBitmap(*my_icon,pos.x, pos.y);
  /*
  int n_pts_ = 8;
  int i=0;
  wxPoint poly_[8];

  poly_[0]=wxPoint(0, 110);
  poly_[1]=wxPoint(30,130);
  poly_[2]=wxPoint(90,130);
  poly_[3]=wxPoint(120,110);
  poly_[4]=wxPoint(120,160);
  poly_[5]=wxPoint(90,140);
  poly_[6]=wxPoint(30,140);
  poly_[7]=wxPoint(0,160);

  wxBrush old_brush=dc->GetBrush();
  wxPen old_pen = dc->GetPen();

  dc->SetBrush(*wxLIGHT_GREY_BRUSH);
  wxCoord xoff = pos.x;
  wxCoord yoff = pos.y;
  dc->DrawPolygon(n_pts_, poly_, xoff, yoff);

  wxPen mypen("MAGENTA",4, wxSOLID);
  dc->SetPen(mypen);

  dc->DrawLine(xoff+130, yoff+135, xoff+160, yoff+135);
  dc->DrawLine(xoff+120, yoff+100, xoff+120, yoff+80);

  dc->SetPen(old_pen);
  n_pts_=4;
  poly_[0] = wxPoint(100, 0);
  poly_[1] = wxPoint(140, 0);
  poly_[2] = wxPoint(140, 70);
  poly_[3] = wxPoint(100, 70);

  wxColour* color;
  color = wxTheColourDatabase->FindColour("SKY BLUE");
  wxBrush mybrush((*color), wxSOLID);
  dc->SetBrush(mybrush);
  dc->DrawPolygon(n_pts_, poly_, xoff, yoff);
  
  for (i=1; i<7; i++) 
  dc->DrawLine(xoff+100, yoff+i*10, xoff+140, yoff+i*10);

  dc->SetBrush(*wxWHITE_BRUSH);

  n_pts_=4;
  poly_[0] = wxPoint(170, 30);
  poly_[1] = wxPoint(190, 30);
  poly_[2] = wxPoint(200, 50);
  poly_[3] = wxPoint(160, 50);

  dc->DrawPolygon(n_pts_, poly_, xoff, yoff);

  dc->SetPen(mypen);
  dc->DrawLine(xoff+150, yoff+10, xoff+180, yoff+10);
  dc->DrawLine(xoff+180, yoff+10, xoff+180, yoff+20);
  dc->DrawLine(xoff+180, yoff+60, xoff+180, yoff+70);
  dc->DrawLine(xoff+150, yoff+70, xoff+180, yoff+70);

  dc->SetPen(old_pen);  
  dc->SetBrush(*wxRED_BRUSH);
  dc->DrawCircle(xoff+185,yoff+135, 15);
  
  wxPen mypen2("BLUE",3, wxSOLID);
  dc->SetPen(mypen2);
  dc->DrawLine(xoff+185, yoff+120, xoff+178, yoff+135);
  dc->DrawLine(xoff+178, yoff+135, xoff+192, yoff+135);
  dc->DrawLine(xoff+192, yoff+135, xoff+185, yoff+150);
  
  dc->SetPen(old_pen);  
  dc->SetBrush(old_brush);
  */
}

void Power::GetIPorts(POLY &iports)
{
  iports[0] = wxPoint(48, 73);
  //iports[0]=wxPoint(icon_w/8-3, icon_h*9/20-3);
}

void Power::GetOPorts(POLY &oports)
{
  oports[0] = wxPoint(144,3);
  //oports[0] = wxPoint(icon_w*7/8+5, icon_h*9/20-3);
    
}
