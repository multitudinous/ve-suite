
#pragma warning(disable:4786)
#pragma warning(disable : 4101)
#pragma warning(disable : 4503)
#pragma warning(disable : 4251)


#include "WGSR.h"
#include "wgsr_UI.h"

IMPLEMENT_DYNAMIC_CLASS(WGSR, REI_Plugin)

WGSR::WGSR()
{
	
    // defaults here
	co_conv_eff = 95.0;
	steam_added = 0.99;
	
	RegistVar("co_conv_eff", &co_conv_eff);
	RegistVar("steam_added", &steam_added);
	
	//  n_pts = 4;
	//poly[0]=wxPoint(0,0);
	//poly[1]=wxPoint(30,0);
	//poly[2]=wxPoint(30,60);
	//poly[3]=wxPoint(0,60);
	wxString wgsr_icon_file="wgsr.jpg";
	wxImage my_img(wgsr_icon_file, wxBITMAP_TYPE_JPEG);
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

wxString WGSR::GetName()
{
	return _T("CMU_IECM_WGSR");
}

wxString WGSR::GetDesc()
{
	return _T("Water Gasi Shift Reactor");
}

UIDialog* WGSR::UI(wxWindow* parent)
{
	if (dlg!=NULL)
		return dlg;
	
	dlg = new WGSR_UI_Dialog(parent, -1, &co_conv_eff, &steam_added);
	
	return dlg;
}

void WGSR::DrawIcon(wxDC* dc)
{
	dc->DrawBitmap(*my_icon,pos.x, pos.y);
	/*
	wxBrush old_brush=dc->GetBrush();
	wxColour* color;
	color = wxTheColourDatabase->FindColour("PURPLE");
	wxBrush mybrush((*color), wxSOLID);
	wxPoint poly_[4];
	poly_[0]=wxPoint(0,10);
	poly_[1]=wxPoint(30,10);
	poly_[2]=wxPoint(30,50);
	poly_[3]=wxPoint(0,50);
	dc->SetBrush(mybrush);
	wxCoord xoff = pos.x;
	wxCoord yoff = pos.y;
	//  dc->DrawRoundedRectangle(xoff, yoff,30,60,0.7);
	//dc->DrawLine(xoff, yoff+10, xoff, yoff+50);
	dc->DrawArc(xoff+30, yoff+10, xoff, yoff+10, xoff+15, yoff+40);
	//  dc->DrawLine(xoff+30, yoff+10, xoff+30, yoff+50);
	dc->DrawArc(xoff, yoff+50,  xoff+30, yoff+50, xoff+15, yoff+20 );
	dc->DrawPolygon(n_pts, poly_, xoff, yoff);
	//  dc->FloodFill(xoff+15, yoff+30, *color);
	//  dc->FloodFill(xoff+15, yoff+55, *color, wxFLOOD_BORDER);
	dc->SetBrush(old_brush);
	*/
}

void WGSR::GetIPorts(POLY &iports)
{
	iports[0]=wxPoint(icon_w*1/10+2, icon_h/2);
}

void WGSR::GetOPorts(POLY &oports)
{
	oports[0] = wxPoint(icon_w*9/10-2, icon_h/2);
}
