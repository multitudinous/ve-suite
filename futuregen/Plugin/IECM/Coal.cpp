#ifdef WIN32
#pragma warning(disable:4786)
#pragma warning(disable : 4101)
#pragma warning(disable : 4503)
#pragma warning(disable : 4251)
#endif

#include "Coal.h"
#include "FuelDialog.h"

IMPLEMENT_DYNAMIC_CLASS(Coal, REI_Plugin)

Coal::Coal()
{
	ulti_c_d=0;
	ulti_h_d=0;
	ulti_o_d=0;
	ulti_n_d=0;
	ulti_s_d=0;
	ulti_cl_d=0;
	ulti_ash_d=0;
	prox_h2o_d=0;
	prox_vm_d=0;
	prox_ash_d=0;
	prox_fc_d=0;
	ashc_sio2_d=0;
	ashc_al2o3_d=0;
	ashc_tio2_d=0;
	ashc_fe2o3_d=0;
	ashc_cao_d=0;
	ashc_mgo_d=0;
	ashc_na2o_d=0;
	ashc_k2o_d=0;
	ashc_so3_d=0;
	ashc_p2o5_d=0;
	ashc_bao_d=0;
	ashc_sro_d=0;
	hhv_d=0;
	coal_cost_d=0;
	coal_name_s="Noname Coal";
	
	// register variables
	RegistVar("ulti_c_d",&ulti_c_d);
	RegistVar("ulti_h_d",&ulti_h_d);
	RegistVar("ulti_o_d",&ulti_o_d);
	RegistVar("ulti_n_d",&ulti_n_d);
	RegistVar("ulti_s_d",&ulti_s_d);
	RegistVar("ulti_cl_d", &ulti_cl_d);
	RegistVar("ulti_ash_d", &ulti_ash_d);
	RegistVar("prox_h2o_d", &prox_h2o_d);
	RegistVar("prox_vm_d", &prox_vm_d);
	RegistVar("prox_ash_d", &prox_ash_d);
	RegistVar("prox_fc_d", &prox_fc_d);
	RegistVar("ashc_sio2_d", &ashc_sio2_d);
	RegistVar("ashc_al2o3_d", &ashc_al2o3_d);
	RegistVar("ashc_tio2_d", &ashc_tio2_d);
	RegistVar("ashc_fe2o3_d", &ashc_fe2o3_d);
	RegistVar("ashc_cao_d", &ashc_cao_d);
	RegistVar("ashc_mgo_d", &ashc_mgo_d);
	RegistVar("ashc_na2o_d", &ashc_na2o_d);
	RegistVar("ashc_k2o_d", &ashc_k2o_d);
	RegistVar("ashc_so3_d", &ashc_so3_d);
	RegistVar("ashc_p2o5_d", &ashc_p2o5_d);
	RegistVar("ashc_bao_d", &ashc_bao_d);
	RegistVar("ashc_sro_d", &ashc_sro_d);
	RegistVar("hhv_d", &hhv_d);
	RegistVar("coal_cost_d", &coal_cost_d);
	RegistVar("coal_name_s", &coal_name_s);
	
	n_pts = 4;
	wxString coal_icon_file="coal.jpg";
	wxImage my_img(coal_icon_file, wxBITMAP_TYPE_JPEG);
	icon_w = my_img.GetWidth() / 5;
	icon_h = my_img.GetHeight() / 5;
	my_icon=new wxBitmap(my_img.Scale(icon_w, icon_h));
	
	poly[0]=wxPoint(0,0);
	poly[1]=wxPoint(icon_w,0);
	poly[2]=wxPoint(icon_w,icon_h);
	poly[3]=wxPoint(0,icon_h);
	
}

wxString Coal::GetName()
{
	return _T("GLOBAL_COAL");
}

wxString Coal::GetDesc()
{
	return _T("This is the Coal Used");
}

UIDialog* Coal::UI(wxWindow* parent)
{
	if (dlg!=NULL)
		return dlg;
	
	dlg = new FuelDialog(parent, -1,
		&coal_name_s,
		&ulti_c_d,
		&ulti_h_d,
		&ulti_o_d,
		&ulti_n_d,
		&ulti_s_d,
		&ulti_cl_d,
		&ulti_ash_d,
		&prox_h2o_d,
		&prox_vm_d,
		&prox_ash_d,
		&prox_fc_d,
		&ashc_sio2_d,
		&ashc_al2o3_d,
		&ashc_tio2_d,
		&ashc_fe2o3_d,
		&ashc_cao_d,
		&ashc_mgo_d,
		&ashc_na2o_d,
		&ashc_k2o_d,
		&ashc_so3_d,
		&ashc_p2o5_d,
		&ashc_bao_d,
		&ashc_sro_d,
		&hhv_d,
		&coal_cost_d);
	return dlg;
}

void Coal::DrawIcon(wxDC* dc)
{
	dc->DrawBitmap(*my_icon,pos.x, pos.y);
	//  wxBrush old_brush=dc->GetBrush();
	//  dc->SetBrush(*wxBLUE_BRUSH);
	//  wxCoord xoff = pos.x;
	//  wxCoord yoff = pos.y;
	//  dc->DrawPolygon(n_pts, poly, xoff, yoff);
	//  dc->SetBrush(old_brush);
}

void Coal::GetOPorts(POLY &oports)
{
	
	oports[0] = wxPoint(icon_w*14/15, icon_h*1/2);
    
}
