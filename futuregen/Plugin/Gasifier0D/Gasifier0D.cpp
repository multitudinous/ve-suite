
#pragma warning(disable:4786)
#pragma warning(disable : 4101)
#pragma warning(disable : 4503)
#pragma warning(disable : 4251)

#include "Gasifier0D.h"
#include "Gasifier0D_UI.h"

IMPLEMENT_DYNAMIC_CLASS(Gasifier0D, REI_Plugin)

/////////////////////////////////////////////////////////////////////////////
Gasifier0D
::Gasifier0D()
{
  RegistVar("steam_temp1", &steam_temp1);
  RegistVar("steam_flrt1", &steam_flrt1);
  RegistVar("slurry_temp1", &slurry_temp1);
  RegistVar("slurry_flrt1", &slurry_flrt1);
  RegistVar("coal_percent1", &coal_percent1);
  RegistVar("steam_temp2", &steam_temp2);
  RegistVar("steam_flrt2", &steam_flrt2);
  RegistVar("slurry_temp2", &slurry_temp2);
  RegistVar("slurry_flrt2", &slurry_flrt2);
  RegistVar("coal_percent2", &coal_percent2);
  RegistVar("steam_temp3", &steam_temp3);
  RegistVar("steam_flrt3", &steam_flrt3);
  RegistVar("slurry_temp3", &slurry_temp3);
  RegistVar("slurry_flrt3", &slurry_flrt3);
  RegistVar("coal_percent3", &coal_percent3);
  RegistVar("geo_diam", &geo_diam);
  RegistVar("geo_stage1_len", &geo_stage1_len);
  RegistVar("geo_stage2_len", &geo_stage2_len);
  RegistVar("geo_stage1_wall", &geo_stage1_wall);
  RegistVar("geo_stage2_wall", &geo_stage2_wall);
  RegistVar("burn_out", &burn_out);
  RegistVar("stage1_heatloss", &stage1_heatloss);
  RegistVar("stage2_heatloss", &stage2_heatloss);
  RegistVar("LD_ratio", &LD_ratio);
  RegistVar("stage1_emis", &stage1_emis);
  RegistVar("stage2_emis", &stage2_emis);
  RegistVar("backside_temp", &backside_temp);
  RegistVar("slag_eff", &slag_eff);
  RegistVar("pres_drop", &pres_drop);
  RegistVar("stage", &stage);
  RegistVar("spec_geometry", &spec_geometry);
  RegistVar("des_mode", &des_mode);
  RegistVar("coal_type", &coal_type);
  RegistVar("size_50", &size_50);
  RegistVar("size_200", &size_200);
  
  stage = 1;
  steam_temp1 = steam_temp2 = steam_temp3 = 0.0;
  steam_flrt1 = steam_flrt2 = steam_flrt3 = 0.0;
  slurry_temp1 = slurry_temp2 = slurry_temp3 = 422.0;
  slurry_flrt1 = slurry_flrt2 = slurry_flrt3= 43.46;
  
  spec_geometry = 0;
  geo_diam = 2.2;
  geo_stage1_len = 5.5;
  geo_stage2_len = 1;
  geo_stage1_wall = 1.0;
  geo_stage2_wall = 1.0;
  burn_out = 98.0;
  stage1_heatloss = 0.0;
  stage2_heatloss = 0.0;
  des_mode = 0;
  LD_ratio = 3.0;
  stage1_emis = 0.75;
  stage2_emis = 0.75;
  backside_temp = 300;
  slag_eff = 50;
  pres_drop = 0.0;

  size_50 = 99.9;
  size_200 = 90.0;
  wxString icon_file="Icons/gasifier2.gif";
  wxImage my_img(icon_file, wxBITMAP_TYPE_GIF);
  icon_w = my_img.GetWidth();
  icon_h = my_img.GetHeight();
  my_icon=new wxBitmap(my_img.Scale(icon_w, icon_h));

  coal_type = "Illinois_#6";

  n_pts = 4;

  poly[0]=wxPoint(0,0);
  poly[1]=wxPoint(icon_w,0);
  poly[2]=wxPoint(icon_w,icon_h);
  poly[3]=wxPoint(0,icon_h);
}



/////////////////////////////////////////////////////////////////////////////
Gasifier0D
::~Gasifier0D()
{

}

/////////////////////////////////////////////////////////////////////////////
double Gasifier0D::GetVersion()
{
  double result=1.0;

  //Your code

  return result;
}

/////////////////////////////////////////////////////////////////////////////
int Gasifier0D::GetNumPoly()
{
  int result=0;
  //Your code
  return n_pts;
}

/////////////////////////////////////////////////////////////////////////////
//void Gasifier0D::GetPoly(POLY &polygon)
//{
//  return ;//polygon;
//}

/////////////////////////////////////////////////////////////////////////////
int Gasifier0D::GetNumIports()
{
  int result=3;

  return result;
}

/////////////////////////////////////////////////////////////////////////////
void Gasifier0D::GetIPorts(POLY &iports)
{
  iports[0]=wxPoint(icon_w*26/63, icon_h*86/123);
  iports[1]=wxPoint(icon_w*20/63, icon_h*105/123);
  iports[2]=wxPoint(icon_w*36/63, icon_h*118/123);

  return;
}

/////////////////////////////////////////////////////////////////////////////
int Gasifier0D::GetNumOports()
{
  int result=1;
  //Your code
  return result;
}

/////////////////////////////////////////////////////////////////////////////
void Gasifier0D::GetOPorts(POLY &oports)
{
  oports[0]=wxPoint(icon_w*37/63, icon_h*20/123);
}

/////////////////////////////////////////////////////////////////////////////
void Gasifier0D::DrawIcon(wxDC* dc)
{
  //Your implementation
  /*wxBrush old_brush=dc->GetBrush();
  dc->SetBrush(*wxBLUE_BRUSH);
  wxCoord xoff = pos.x;
  wxCoord yoff = pos.y;
  dc->DrawPolygon(n_pts, poly, xoff, yoff);
  dc->SetBrush(old_brush);*/
  dc->DrawBitmap(*my_icon,pos.x, pos.y);
}

/////////////////////////////////////////////////////////////////////////////
UIDialog* Gasifier0D::UI(wxWindow* parent)
{
  if (dlg!=NULL)
    return dlg;
  
  dlg = new Gasifier0D_UI_Dialog (parent, -1,
     &steam_temp1,
     &steam_flrt1,
     &slurry_temp1,
     &slurry_flrt1,
     &coal_percent1,
     &steam_temp2,
     &steam_flrt2,
     &slurry_temp2,
     &slurry_flrt2,
     &coal_percent2,
     &steam_temp3,
     &steam_flrt3,
     &slurry_temp3,
     &slurry_flrt3,
     &coal_percent3,
     &geo_diam,
     &geo_stage1_len,
     &geo_stage2_len,
     &geo_stage1_wall,
     &geo_stage2_wall,
     &burn_out,
     &stage1_heatloss,
     &stage2_heatloss,
     &LD_ratio,
     &stage1_emis,
     &stage2_emis,
     &backside_temp,
     &slag_eff,
     &pres_drop,				
     &stage,
     &spec_geometry,
     &des_mode,
	 &coal_type,
	 &size_50,
	 &size_200);
     
  return dlg;
}

/////////////////////////////////////////////////////////////////////////////
wxString Gasifier0D::GetName()
{
  wxString result="REI_LarryRuth_Gasifier0D"; //your name
  return result;
}

/////////////////////////////////////////////////////////////////////////////
wxString Gasifier0D::GetDesc()
{
  wxString result="Gasifier0D Module by REI"; //your description

  return result;
}


