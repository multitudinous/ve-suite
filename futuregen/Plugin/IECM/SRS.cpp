#pragma warning(disable:4786)
#pragma warning(disable : 4101)
#pragma warning(disable : 4503)
#pragma warning(disable : 4251)


#include "SRS.h"
#include "SRS_UI.h"

IMPLEMENT_DYNAMIC_CLASS(SRS, REI_Plugin)

SRS::SRS()
{

	// put defaults here
	tail_gas_sre_d = 99.0;
	h2s_rmv_eff_d  = 95.0;
	sulf_rcv_eff_d = 95.0;
	
	// register variable with plugin_base class - these will then 
	// be packaged into a string to send across interface
	RegistVar("tail_gas_sre", &tail_gas_sre_d);
	RegistVar("h2s_rmv_eff", &h2s_rmv_eff_d);
	RegistVar("sulf_rcv_eff", &sulf_rcv_eff_d);
	
	if (poly!=NULL)
		delete poly;
	
		/*  poly = new wxPoint[6];
		n_pts = 6;
		
		 poly[0]=wxPoint(20,0);
		 poly[1]=wxPoint(40,10);
		 poly[2]=wxPoint(40,50);
		 poly[3]=wxPoint(20,60);
		 poly[4]=wxPoint(0,50);
		 poly[5]=wxPoint(0,10);
	*/
	
	wxString srs_icon_file="srs.jpg";
	wxImage my_img(srs_icon_file, wxBITMAP_TYPE_JPEG);
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

//////////////////////////////////////////////////////////////////////
wxString SRS::GetName()
{
	return _T("CMU_IECM_SRS");
}

//////////////////////////////////////////////////////////////////////
wxString SRS::GetDesc()
{
	return _T("Sulfur Removal System");
}

//////////////////////////////////////////////////////////////////////
UIDialog* SRS::UI(wxWindow* parent)
{
	if (dlg!=NULL)
		return dlg;
	
	dlg = new SRS_UI_Dialog(parent, -1, &tail_gas_sre_d, &h2s_rmv_eff_d, &sulf_rcv_eff_d);
	
	return dlg;
}

//////////////////////////////////////////////////////////////////////
void SRS::DrawIcon(wxDC* dc)
{
	dc->DrawBitmap(*my_icon,pos.x, pos.y);
	/*
	int n_pts_ = 5;
	wxPoint poly_[5];
	
	 poly_[0]=poly[0];
	 poly_[1]=poly[1];
	 poly_[2]=wxPoint(40,30);
	 poly_[3]=wxPoint(0,30);
	 poly_[4]=poly[5];
	 
	  wxBrush old_brush=dc->GetBrush();
	  dc->SetBrush(*wxRED_BRUSH);
	  wxCoord xoff = pos.x;
	  wxCoord yoff = pos.y;
	  dc->DrawPolygon(n_pts_, poly_, xoff, yoff);
	  
	   wxColour* color;
	   color = wxTheColourDatabase->FindColour("YELLOW");
	   wxBrush mybrush((*color), wxSOLID);
	   dc->SetBrush(mybrush);
	   
		poly_[0]=wxPoint(0,30);
		poly_[1]=wxPoint(40,30);
		poly_[2]=poly[2];
		poly_[3]=poly[3];
		poly_[4]=poly[4];
		
		 dc->DrawPolygon(n_pts_, poly_, xoff, yoff);
		 
		  dc->SetBrush(old_brush);
	*/
}

void SRS::GetIPorts(POLY &iports)
{ 
	iports[0]=wxPoint(icon_w/3-1, icon_h/2);
	
}

void SRS::GetOPorts(POLY &oports)
{
	oports[0] = wxPoint(icon_w*2/3+3,icon_h/2);
	
}
