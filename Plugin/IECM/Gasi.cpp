#pragma warning(disable:4786)
#pragma warning(disable : 4101)
#pragma warning(disable : 4503)
#pragma warning(disable : 4251)


#include "Gasi.h"
#include "Gasi_UI.h"
#include <wx/gdicmn.h>
IMPLEMENT_DYNAMIC_CLASS(GASI, REI_Plugin)

GASI::GASI()
{
	
	tex_idx_in_ntrains = 1;
	tex_idx_in_nspares = 1;
	tex_idx_in_temp = 2450;
	wc_ratio = 0.5;
	
	RegistVar("tex_idx_in_ntrains", &tex_idx_in_ntrains);
	RegistVar("tex_idx_in_nspares", &tex_idx_in_nspares);
	RegistVar("tex_idx_in_temp", &tex_idx_in_temp);
	RegistVar("tex_idx_in_wc_ratio", &wc_ratio);
	
	if (poly!=NULL)
		delete poly;
	
	wxString gasi_icon_file="gassifier.jpg";
	wxImage my_img(gasi_icon_file, wxBITMAP_TYPE_JPEG);
	icon_w = my_img.GetWidth() / 5;
	icon_h = my_img.GetHeight() / 5;
	my_icon=new wxBitmap(my_img.Scale(icon_w, icon_h));
	
	poly = new wxPoint[4];
	n_pts = 4;
	
	//  my_icon->SetWidth(icon_w);
	//my_icon->SetHeight(icon_h);
	
	poly[0]=wxPoint(0,0);
	poly[1]=wxPoint(icon_w,0);
	poly[2]=wxPoint(icon_w,icon_h);
	poly[3]=wxPoint(0,icon_h);
	/*  poly[0]=wxPoint(0,0);
	poly[1]=wxPoint(55,0);
	poly[2]=wxPoint(55,10);
	poly[3]=wxPoint(50,10);
	poly[4]=wxPoint(50,30);
	poly[5]=wxPoint(45,30);
	poly[6]=wxPoint(45,60);
	poly[7]=wxPoint(40,60);
	poly[8]=wxPoint(40,90);
	poly[9]=wxPoint(35,90);
	poly[10]=wxPoint(10,80);
	poly[11]=wxPoint(10,60);
	poly[12]=wxPoint(5,60);
	poly[13]=wxPoint(5,30);
	poly[14]=wxPoint(0,30);
	*/
}

wxString GASI::GetName()
{
	return _T("CMU_IECM_GASIFIER");
}

wxString GASI::GetDesc()
{
	return _T("Gasifier Area");
}

UIDialog* GASI::UI(wxWindow* parent)
{
	if (dlg!=NULL)
		return dlg;
	
	dlg = new GASI_UI_Dialog(parent, -1, &tex_idx_in_ntrains, &tex_idx_in_nspares, &tex_idx_in_temp, &wc_ratio);
	
	return dlg;
}

void GASI::DrawIcon(wxDC* dc)
{
	dc->DrawBitmap(*my_icon,pos.x, pos.y);
	/* original polygon representation
    wxBrush old_brush=dc->GetBrush();
    wxColour* color;
    
	 color = wxTheColourDatabase->FindColour("YELLOW");
	 wxBrush mybrush((*color), wxSOLID);
	 
	  dc->SetBrush(mybrush);
	  wxCoord xoff = pos.x;
	  wxCoord yoff = pos.y;
	  dc->DrawPolygon(n_pts, poly, xoff, yoff);
	  dc->SetBrush(old_brush);
	*/
	
	
}

void GASI::GetIPorts(POLY &iports)
{
	iports[0]=wxPoint(icon_w*2/5, icon_h/10);
}

void GASI::GetOPorts(POLY &oports)
{
	
	oports[0] = wxPoint(icon_w*9/10-2,icon_h/2);
    
}
