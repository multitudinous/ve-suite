
#pragma warning(disable:4786)
#pragma warning(disable : 4101)
#pragma warning(disable : 4503)
#pragma warning(disable : 4251)

#include "WaterPump.h"
#include "WaterPump_UI.h"

IMPLEMENT_DYNAMIC_CLASS(WaterPump, REI_Plugin)

/////////////////////////////////////////////////////////////////////////////
WaterPump
::WaterPump()
{
  RegistVar("eff", &eff);
  RegistVar("pressure_out", &pressure_out);
  RegistVar("pressure_change", &pressure_change);
  RegistVar("case_type", &case_type);

  eff = 0.82;
  pressure_out = 0.0;
  pressure_change = 2067854;
  case_type = 0;
}



/////////////////////////////////////////////////////////////////////////////
WaterPump
::~WaterPump()
{

}

/////////////////////////////////////////////////////////////////////////////
double WaterPump::GetVersion()
{
  double result=1.0;

  //Your code

  return result;
}

/////////////////////////////////////////////////////////////////////////////
int WaterPump::GetNumPoly()
{
  int result=0;
  //Your code
  return n_pts;
}

/////////////////////////////////////////////////////////////////////////////
//void WaterPump::GetPoly(POLY &polygon)
//{
//  return ;//polygon;
//}

/////////////////////////////////////////////////////////////////////////////
int WaterPump::GetNumIports()
{
  int result=1;

  return result;
}

/////////////////////////////////////////////////////////////////////////////
void WaterPump::GetIPorts(POLY &iports)
{
  iports[0]=wxPoint(0,20);
  return;
}

/////////////////////////////////////////////////////////////////////////////
int WaterPump::GetNumOports()
{
  int result=1;
  //Your code
  return result;
}

/////////////////////////////////////////////////////////////////////////////
void WaterPump::GetOPorts(POLY &oports)
{
  oports[0]=wxPoint(40,20); 
}

/////////////////////////////////////////////////////////////////////////////
void WaterPump::DrawIcon(wxDC* dc)
{
  //Your implementation
  wxBrush old_brush=dc->GetBrush();
  dc->SetBrush(*wxWHITE_BRUSH);
  wxCoord xoff = pos.x;
  wxCoord yoff = pos.y;
  dc->DrawPolygon(n_pts, poly, xoff, yoff);
  dc->SetBrush(old_brush);
}

/////////////////////////////////////////////////////////////////////////////
UIDialog* WaterPump::UI(wxWindow* parent)
{
  if (dlg!=NULL)
    return dlg;
  
  dlg = new WaterPump_UI_Dialog (parent, -1,
     &eff,
     &pressure_out,
     &pressure_change,
     &case_type);
      
  return dlg;
}

/////////////////////////////////////////////////////////////////////////////
wxString WaterPump::GetName()
{
  wxString result="REI_LarryRuth_WaterPump"; //your name
  return result;
}

/////////////////////////////////////////////////////////////////////////////
wxString WaterPump::GetDesc()
{
  wxString result="Water Pump Module by REI"; //your description

  return result;
}


