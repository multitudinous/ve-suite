
#pragma warning(disable:4786)
#pragma warning(disable : 4101)
#pragma warning(disable : 4503)
#pragma warning(disable : 4251)

#include "SteamTurbine.h"
#include "SteamTurbine_UI.h"

IMPLEMENT_DYNAMIC_CLASS(SteamTurbine, REI_Plugin)

/////////////////////////////////////////////////////////////////////////////
SteamTurbine
::SteamTurbine()
{
  RegistVar("ad_eff", &ad_eff);
  RegistVar("pressure_drop", &pressure_drop);

  ad_eff = 0.82;
  pressure_drop = 2067854;

  n_pts = 3;
}



/////////////////////////////////////////////////////////////////////////////
SteamTurbine
::~SteamTurbine()
{

}

/////////////////////////////////////////////////////////////////////////////
double SteamTurbine::GetVersion()
{
  double result=1.0;

  //Your code

  return result;
}

/////////////////////////////////////////////////////////////////////////////
int SteamTurbine::GetNumPoly()
{
  int result=0;
  //Your code
  return n_pts;
}

/////////////////////////////////////////////////////////////////////////////
//void SteamTurbine::GetPoly(POLY &polygon)
//{
//  return ;//polygon;
//}

/////////////////////////////////////////////////////////////////////////////
int SteamTurbine::GetNumIports()
{
  int result=1;

  return result;
}

/////////////////////////////////////////////////////////////////////////////
void SteamTurbine::GetIPorts(POLY &iports)
{
  iports[0]=wxPoint(0,20);
  return;
}

/////////////////////////////////////////////////////////////////////////////
int SteamTurbine::GetNumOports()
{
  int result=1;
  //Your code
  return result;
}

/////////////////////////////////////////////////////////////////////////////
void SteamTurbine::GetOPorts(POLY &oports)
{
  oports[0]=wxPoint(40,20);  
}

/////////////////////////////////////////////////////////////////////////////
void SteamTurbine::DrawIcon(wxDC* dc)
{
  //Your implementation
  wxBrush old_brush=dc->GetBrush();
  dc->SetBrush(*wxCYAN_BRUSH);
  wxCoord xoff = pos.x;
  wxCoord yoff = pos.y;
  dc->DrawPolygon(n_pts, poly, xoff, yoff);
  dc->SetBrush(old_brush);
}

/////////////////////////////////////////////////////////////////////////////
UIDialog* SteamTurbine::UI(wxWindow* parent)
{
  if (dlg!=NULL)
    return dlg;
  
  dlg = new SteamTurbine_UI_Dialog (parent, -1,
     &ad_eff,
     &pressure_drop);
      
  return dlg;
}

/////////////////////////////////////////////////////////////////////////////
wxString SteamTurbine::GetName()
{
  wxString result="REI_LarryRuth_SteamTurbine"; //your name
  return result;
}

/////////////////////////////////////////////////////////////////////////////
wxString SteamTurbine::GetDesc()
{
  wxString result="Steam Turbine Module by REI"; //your description

  return result;
}


