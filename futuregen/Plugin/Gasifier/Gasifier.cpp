
#pragma warning(disable:4786)
#pragma warning(disable : 4101)
#pragma warning(disable : 4503)
#pragma warning(disable : 4251)

#include "Gasifier.h"
#include "Gasifier_UI.h"

IMPLEMENT_DYNAMIC_CLASS(Gasifier, REI_Plugin)

/////////////////////////////////////////////////////////////////////////////
Gasifier
::Gasifier()
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
  RegistVar("pres_drop", &slag_eff);
  RegistVar("stage", &stage);
  RegistVar("spec_geometry", &spec_geometry);
  RegistVar("des_mode", &des_mode);

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
  pres_drop = 524691;

  n_pts = 4;
  poly[3]= wxPoint(20, 40);
}



/////////////////////////////////////////////////////////////////////////////
Gasifier
::~Gasifier()
{

}

/////////////////////////////////////////////////////////////////////////////
double Gasifier::GetVersion()
{
  double result=1.0;

  //Your code

  return result;
}

/////////////////////////////////////////////////////////////////////////////
int Gasifier::GetNumPoly()
{
  int result=0;
  //Your code
  return n_pts;
}

/////////////////////////////////////////////////////////////////////////////
//void Gasifier::GetPoly(POLY &polygon)
//{
//  return ;//polygon;
//}

/////////////////////////////////////////////////////////////////////////////
int Gasifier::GetNumIports()
{
  int result=1;

  return result;
}

/////////////////////////////////////////////////////////////////////////////
void Gasifier::GetIPorts(POLY &iports)
{
  iports[0]=wxPoint(0, 20);

  return;
}

/////////////////////////////////////////////////////////////////////////////
int Gasifier::GetNumOports()
{
  int result=1;
  //Your code
  return result;
}

/////////////////////////////////////////////////////////////////////////////
void Gasifier::GetOPorts(POLY &oports)
{
  oports[0]=wxPoint(40, 20);
}

/////////////////////////////////////////////////////////////////////////////
void Gasifier::DrawIcon(wxDC* dc)
{
  //Your implementation
  wxBrush old_brush=dc->GetBrush();
  dc->SetBrush(*wxBLUE_BRUSH);
  wxCoord xoff = pos.x;
  wxCoord yoff = pos.y;
  dc->DrawPolygon(n_pts, poly, xoff, yoff);
  dc->SetBrush(old_brush);
}

/////////////////////////////////////////////////////////////////////////////
UIDialog* Gasifier::UI(wxWindow* parent)
{
  if (dlg!=NULL)
    return dlg;
  
  dlg = new Gasifier_UI_Dialog (parent, -1,
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
     &des_mode);
      
  return dlg;
}

/////////////////////////////////////////////////////////////////////////////
wxString Gasifier::GetName()
{
  wxString result="REI_LarryRuth_Gasifier"; //your name
  return result;
}

/////////////////////////////////////////////////////////////////////////////
wxString Gasifier::GetDesc()
{
  wxString result="Gasifier Module by REI"; //your description

  return result;
}


