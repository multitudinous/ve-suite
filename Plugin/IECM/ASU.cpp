#pragma warning(disable:4786)
#pragma warning(disable : 4101)
#pragma warning(disable : 4503)
#pragma warning(disable : 4251)


#include "ASU.h"
#include "ASU_UI.h"

IMPLEMENT_DYNAMIC_CLASS(ASU, REI_Plugin)

ASU::ASU()
{
	
  // defaults here
  asu_idx_in_nspares = 0;
  o2_purity = 95.0;
  
  // register variables for packing
  RegistVar("asu_idx_in_nspares", &asu_idx_in_nspares);
  RegistVar("o2_purity", &o2_purity);
  
  n_pts = 4;
  /*
    wxString asu_icon_file="asu.jpg";
    wxImage my_img(asu_icon_file, wxBITMAP_TYPE_JPEG);
    icon_w = my_img.GetWidth() / 5;
    icon_h = my_img.GetHeight() / 5;
    my_icon=new wxBitmap(my_img.Scale(icon_w, icon_h));
    
    poly[0]=wxPoint(0,0);
    poly[1]=wxPoint(icon_w,0);
    poly[2]=wxPoint(icon_w,icon_h);
    poly[3]=wxPoint(0,icon_h);
  */
  
  poly[0]=wxPoint(0,0);
  poly[1]=wxPoint(56,0);
  poly[2]=wxPoint(56,40);
  poly[3]=wxPoint(0,40);
}

wxString ASU::GetName()
{
	return _T("CMU_IECM_ASU");
}

wxString ASU::GetDesc()
{
	return _T("Air Separation Unit");
}

UIDialog* ASU::UI(wxWindow* parent)
{
	if (dlg!=NULL)
		return dlg;
	
	dlg = new ASU_UI_Dialog(parent, -1, &asu_idx_in_nspares, &o2_purity);
	
	return dlg;
}

void ASU::DrawIcon(wxDC* dc)
{
  // dc->DrawBitmap(*my_icon,pos.x, pos.y);
  wxBrush old_brush=dc->GetBrush();
  dc->SetBrush(*wxGREEN_BRUSH);
  wxCoord xoff = pos.x;
  wxCoord yoff = pos.y;
  dc->DrawPolygon(n_pts, poly, xoff, yoff);
  dc->SetBrush(old_brush);
}

void ASU::GetIPorts(POLY &iports)
{
  //iports[0]=wxPoint(icon_w/15, icon_h*20/32);
  iports[0]=wxPoint(0, 20);
}

void ASU::GetOPorts(POLY &oports)
{
  oports[0]=wxPoint(56, 20);
  //oports[0] = wxPoint(icon_w*14/15, icon_h*24/32);
  
}
